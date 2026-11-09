# Distribution packages

| Distribution | File |
|---|---|
| Arch Linux | [`arch/PKGBUILD`](arch/PKGBUILD) |
| Fedora | [`fedora/waymobar.spec`](fedora/waymobar.spec) |

Like xmobar, waymobar compiles `~/.config/waymobar/waymobar.hs` with ghc
at run time, so the packages install the Haskell library too and depend
on ghc and on the development files of cairo, pango, gdk-pixbuf and
libpulse:

* **Arch** ships Haskell libraries as shared objects only. The PKGBUILD
  builds them that way and enables the `dynamic-config` cabal flag, which
  makes waymobar compile the configuration with `-dynamic`.
* **Fedora** splits it like other Haskell packages: `waymobar` (the
  binary) requires `ghc-waymobar-devel` (the library).

## Arch Linux

```sh
cd packaging/arch
makepkg -si
```

## Fedora

```sh
sudo dnf install rpm-build rpmdevtools dnf-plugins-core
sudo dnf builddep packaging/fedora/waymobar.spec
spectool -g -R packaging/fedora/waymobar.spec
rpmbuild -bb packaging/fedora/waymobar.spec
sudo dnf install ~/rpmbuild/RPMS/*/{,ghc-}waymobar*-0.1.0-*.rpm
```

The same spec builds in [COPR](https://copr.fedorainfracloud.org/).

## A new release

1. Bump `version:` in `waymobar.cabal`, `pkgver` in the PKGBUILD and
   `Version:` in the spec (plus a `%changelog` entry).
2. Tag it: `git tag -a v0.2.0 -m "waymobar 0.2.0" && git push --tags`.
3. Update `sha256sums` in the PKGBUILD with `updpkgsums`.
