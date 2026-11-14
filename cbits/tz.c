/* Wall-clock time in a named time zone ("Europe/Madrid") to UTC, with glib
   (already there through pango), so no time zone library is needed. */
#include <glib.h>
#include <stdint.h>

int waymobar_zone_to_utc(const char *name, int year, int month, int day,
                         int hour, int minute, int second, int64_t *out)
{
    /* Not unreferenced: glib keeps one object per name and hands it out
       again, instead of reading the zone file on every call. */
    GTimeZone *tz = g_time_zone_new_identifier(name);
    GDateTime *dt;
    if (!tz)
        return 0;
    dt = g_date_time_new(tz, year, month, day, hour, minute, (gdouble)second);
    if (!dt)
        return 0;
    *out = g_date_time_to_unix(dt);
    g_date_time_unref(dt);
    return 1;
}
