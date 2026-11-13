-- | The weather, from <https://open-meteo.com Open-Meteo> (free, no API
-- key). The place can be given as coordinates, as a name (looked up with
-- Open-Meteo's geocoder) or found from the public IP address. Fetched with
-- @curl@, so no TLS library is needed.
module Waymobar.Weather
  ( Weather (..)
  , WeatherConfig (..)
  , Location (..)
  , Units (..)
  , defaultWeather
  , weather
  , weatherIcon
  , weatherEmoji
  , weatherDescription
    -- * Lower level
  , Place (..)
  , resolveLocation
  , fetchWeather
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forever, unless, void, when)
import qualified Data.ByteString as B
import Data.Char (isAlphaNum)
import Data.IORef
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showFFloat, showHex)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.Process

import Waymobar.Core
import Waymobar.JSON

-- | Where to get the weather for.
data Location
  = Coords Double Double   -- ^ latitude, longitude (@Coords 40.42 (-3.70)@)
  | City String            -- ^ a place name, e.g. @"Madrid"@ or @"Sevilla"@ (Open-Meteo's geocoder)
  | AutoLocation           -- ^ from the public IP address (<https://ip-api.com ip-api.com>)
  deriving (Eq, Show)

data Units
  = Metric     -- ^ °C, km/h
  | Imperial   -- ^ °F, mph
  deriving (Eq, Show)

data WeatherConfig = WeatherConfig
  { weatherLocation :: Location
  , weatherUnits    :: Units
  , weatherInterval :: Double   -- ^ seconds between updates; Open-Meteo's current weather changes every 15 minutes
  , weatherClickRefreshes :: Bool  -- ^ a left click fetches the weather now
  , weatherHover    :: Weather -> String
    -- ^ markup shown instead while the pointer is on the widget (@""@ = none):
    -- the place, feels-like, min/max, humidity, wind...
  }

-- | Located from the IP address, metric, every 15 minutes.
defaultWeather :: WeatherConfig
defaultWeather = WeatherConfig AutoLocation Metric 900 True (const "")

-- | A resolved location.
data Place = Place
  { placeName :: String   -- ^ @""@ for 'Coords'
  , placeLat  :: Double
  , placeLon  :: Double
  } deriving (Eq, Show)

-- | The current weather. Temperatures are °C or °F and the wind km/h or
-- mph, after 'weatherUnits'.
data Weather = Weather
  { weatherPlace     :: String  -- ^ the place name (@""@ with 'Coords')
  , weatherTemp      :: Int
  , weatherFeelsLike :: Int
  , weatherTempMin   :: Int     -- ^ today's minimum
  , weatherTempMax   :: Int     -- ^ today's maximum
  , weatherHumidity  :: Int     -- ^ percent
  , weatherWind      :: Int
  , weatherCode      :: Int     -- ^ WMO weather code: 0 clear, 3 overcast, 61 rain, 95 thunderstorm...
  , weatherIsDay     :: Bool
  } deriving (Eq, Show)

-- | The weather, refreshed every 'weatherInterval' seconds; @f@ gets
-- 'Nothing' until the first answer and when the service has been
-- unreachable for a while (the last reading is kept through short
-- outages). Return @""@ to hide the widget.
--
-- > weather defaultWeather { weatherLocation = City "Madrid" } $ \case
-- >   Nothing -> ""
-- >   Just w -> weatherIcon w ++ " " ++ show (weatherTemp w) ++ "°"
weather :: WeatherConfig -> (Maybe Weather -> String) -> Widget
weather cfg fmt = Widget $ \env -> do
  ref <- newIORef Nothing
  kick <- newTVarIO False
  let set v = do
        old <- atomicModifyIORef' ref (\o -> (v, o))
        unless (old == v) (redraw env)
      -- wait for the timer, or for a click
      wait :: Double -> IO ()
      wait secs = do
        d <- registerDelay (round (secs * 1000000))
        atomically $ (readTVar d >>= check) `orElse` (readTVar kick >>= check >> writeTVar kick False)
      locate = resolveLocation (weatherLocation cfg) >>= \case
        Just p -> pure p
        Nothing -> wait 60 >> locate
  void $ forkIO $ do
    place <- locate
    fails <- newIORef (0 :: Int)
    forever $ do
      r <- fetchWeather (weatherUnits cfg) place
      secs <- case r of
        Just w -> do
          writeIORef fails 0
          set (Just w)
          pure (weatherInterval cfg)
        Nothing -> do
          n <- atomicModifyIORef' fails (\n -> (n + 1, n + 1))
          when (n >= 3) (set Nothing)
          pure 60
      wait secs
  let click c = when (weatherClickRefreshes cfg && clickButton c == LeftButton) $
        atomically (writeTVar kick True)
  pure $ \_ -> do
    mw <- readIORef ref
    let s = fmt mw
        hov = [ h | Just w <- [mw], let h = weatherHover cfg w, not (null h) ]
    pure [ (block s) { blockOnClick = Just click, blockHoverMarkup = listToMaybe hov } | not (null s) ]

-- | Coordinates (and name) for a location; 'Nothing' if the lookup failed.
resolveLocation :: Location -> IO (Maybe Place)
resolveLocation = \case
  Coords lat lon -> pure (Just (Place "" lat lon))
  City name -> do
    r <- httpJson ("https://geocoding-api.open-meteo.com/v1/search?count=1&format=json&name=" ++ urlEncode name)
    pure $ do
      JArr (p : _) <- lookupKey "results" =<< r
      lat <- num "latitude" p
      lon <- num "longitude" p
      pure (Place name lat lon)
  AutoLocation -> do
    r <- httpJson "http://ip-api.com/json/?fields=status,city,lat,lon"
    pure $ do
      o <- r
      JStr "success" <- lookupKey "status" o
      lat <- num "lat" o
      lon <- num "lon" o
      pure (Place (maybe "" id (text' "city" o)) lat lon)

-- | Ask Open-Meteo for the current weather at a place.
fetchWeather :: Units -> Place -> IO (Maybe Weather)
fetchWeather units p = do
  let url = "https://api.open-meteo.com/v1/forecast?latitude=" ++ coord (placeLat p)
         ++ "&longitude=" ++ coord (placeLon p)
         ++ "&current=temperature_2m,relative_humidity_2m,apparent_temperature,is_day,weather_code,wind_speed_10m"
         ++ "&daily=temperature_2m_max,temperature_2m_min&forecast_days=1&timezone=auto"
         ++ (case units of
               Metric -> ""
               Imperial -> "&temperature_unit=fahrenheit&wind_speed_unit=mph")
  r <- httpJson url
  pure $ do
    o <- r
    cur <- lookupKey "current" o
    daily <- lookupKey "daily" o
    temp <- num "temperature_2m" cur
    feels <- num "apparent_temperature" cur
    hum <- num "relative_humidity_2m" cur
    wind <- num "wind_speed_10m" cur
    code <- num "weather_code" cur
    day <- num "is_day" cur
    tmax <- firstNum "temperature_2m_max" daily
    tmin <- firstNum "temperature_2m_min" daily
    pure Weather
      { weatherPlace = placeName p
      , weatherTemp = round temp
      , weatherFeelsLike = round feels
      , weatherTempMin = round tmin
      , weatherTempMax = round tmax
      , weatherHumidity = round hum
      , weatherWind = round wind
      , weatherCode = round code
      , weatherIsDay = day /= 0
      }
  where
    coord x = showFFloat (Just 4) x ""

-- | A Nerd Font glyph (@nf-weather-*@, in the Symbols Nerd Font) for the
-- weather code, day or night.
weatherIcon :: Weather -> String
weatherIcon w = case weatherCode w of
  0 -> dn "\xe30d" "\xe32b"            -- day_sunny / night_clear
  1 -> dn "\xe30c" "\xe379"            -- day_sunny_overcast / night_alt_partly_cloudy
  2 -> dn "\xe302" "\xe37e"            -- day_cloudy / night_alt_cloudy
  3 -> "\xe312"                        -- cloudy
  c | c `elem` [45, 48] -> dn "\xe303" "\xe346"   -- day_fog / night_fog
    | c `elem` [51, 53, 55] -> "\xe31b"            -- sprinkle
    | c `elem` [56, 57, 66, 67] -> "\xe316"        -- rain_mix (freezing)
    | c `elem` [61, 63, 65] -> "\xe318"            -- rain
    | c `elem` [71, 73, 75, 77] -> "\xe31a"        -- snow
    | c `elem` [80, 81, 82] -> dn "\xe309" "\xe326"   -- day_showers / night_alt_showers
    | c `elem` [85, 86] -> dn "\xe30a" "\xe327"       -- day_snow / night_alt_snow
    | c == 95 -> "\xe31d"                          -- thunderstorm
    | c `elem` [96, 99] -> "\xe314"                -- hail
    | otherwise -> "\xe374"                        -- na
  where
    dn d n = if weatherIsDay w then d else n

-- | A colour emoji for the weather code, day or night (needs an emoji
-- font such as Noto Color Emoji; pango falls back to it by itself).
weatherEmoji :: Weather -> String
weatherEmoji w = case weatherCode w of
  0 -> dn "\x2600\xfe0f" "\x1f319"           -- ☀️ / 🌙
  1 -> dn "\x1f324\xfe0f" "\x1f319"          -- 🌤️ / 🌙
  2 -> dn "\x26c5" "\x2601\xfe0f"            -- ⛅ / ☁️
  3 -> "\x2601\xfe0f"                        -- ☁️
  c | c `elem` [45, 48] -> "\x1f32b\xfe0f"                    -- 🌫️
    | c `elem` [51, 53, 55, 56, 57] -> "\x1f326\xfe0f"        -- 🌦️ drizzle
    | c `elem` [61, 63, 65, 66, 67] -> "\x1f327\xfe0f"        -- 🌧️ rain
    | c `elem` [71, 73, 75, 77, 85, 86] -> "\x1f328\xfe0f"    -- 🌨️ snow
    | c `elem` [80, 81, 82] -> dn "\x1f326\xfe0f" "\x1f327\xfe0f"   -- showers
    | c `elem` [95, 96, 99] -> "\x26c8\xfe0f"                 -- ⛈️
    | otherwise -> "\x2753"                                    -- ❓
  where
    dn d n = if weatherIsDay w then d else n

-- | The weather code in words (English).
weatherDescription :: Weather -> String
weatherDescription w = case weatherCode w of
  0 -> "Clear"
  1 -> "Mainly clear"
  2 -> "Partly cloudy"
  3 -> "Overcast"
  45 -> "Fog"
  48 -> "Rime fog"
  51 -> "Light drizzle"
  53 -> "Drizzle"
  55 -> "Heavy drizzle"
  56 -> "Freezing drizzle"
  57 -> "Heavy freezing drizzle"
  61 -> "Light rain"
  63 -> "Rain"
  65 -> "Heavy rain"
  66 -> "Freezing rain"
  67 -> "Heavy freezing rain"
  71 -> "Light snow"
  73 -> "Snow"
  75 -> "Heavy snow"
  77 -> "Snow grains"
  80 -> "Light showers"
  81 -> "Showers"
  82 -> "Heavy showers"
  85 -> "Snow showers"
  86 -> "Heavy snow showers"
  95 -> "Thunderstorm"
  96 -> "Thunderstorm with hail"
  99 -> "Thunderstorm with heavy hail"
  _ -> "Unknown"

------------------------------------------------------------------------------

-- | GET a URL with curl and parse the JSON; 'Nothing' on any failure
-- (no curl, offline, HTTP error, bad JSON).
httpJson :: String -> IO (Maybe JValue)
httpJson url = do
  r <- E.try @E.SomeException $ do
    (_, Just out, _, ph) <- createProcess
      (proc "curl" ["-sfL", "--max-time", "20", "-A", "waymobar", url])
      { std_out = CreatePipe, std_err = NoStream, std_in = NoStream }
    body <- B.hGetContents out
    hClose out
    ec <- waitForProcess ph
    pure (ec, body)
  pure $ case r of
    Right (ExitSuccess, body) -> parseUtf8 body
    _ -> Nothing

num :: String -> JValue -> Maybe Double
num k o = case lookupKey k o of
  Just (JNum n) -> Just n
  _ -> Nothing

firstNum :: String -> JValue -> Maybe Double
firstNum k o = case lookupKey k o of
  Just (JArr (JNum n : _)) -> Just n
  _ -> Nothing

text' :: String -> JValue -> Maybe String
text' k o = case lookupKey k o of
  Just (JStr s) -> Just s
  _ -> Nothing

-- | Percent-encode a query parameter (UTF-8).
urlEncode :: String -> String
urlEncode = concatMap enc . B.unpack . TE.encodeUtf8 . T.pack
  where
    enc b
      | b < 128 && (isAlphaNum c || c `elem` "-._~") = [c]
      | otherwise = '%' : pad (showHex b "")
      where c = toEnum (fromIntegral b)
    pad h = replicate (2 - length h) '0' ++ map toUpperHex h
    toUpperHex ch = if ch >= 'a' && ch <= 'f' then toEnum (fromEnum ch - 32) else ch
