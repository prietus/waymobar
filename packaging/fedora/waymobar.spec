# Haskell packaging guidelines:
# https://docs.fedoraproject.org/en-US/packaging-guidelines/Haskell/

%global pkg_name waymobar
%global pkgver %{pkg_name}-%{version}
%{?haskell_setup}

Name:           %{pkg_name}
Version:        0.1.0
Release:        1%{?dist}
Summary:        Wayland status bar written and configured in Haskell

License:        BSD-3-Clause
URL:            https://github.com/prietus/waymobar
Source0:        %{url}/archive/v%{version}/%{pkgver}.tar.gz

BuildRequires:  ghc-rpm-macros
BuildRequires:  ghc-Cabal-devel
BuildRequires:  ghc-base-devel
BuildRequires:  ghc-binary-devel
BuildRequires:  ghc-bytestring-devel
BuildRequires:  ghc-containers-devel
BuildRequires:  ghc-directory-devel
BuildRequires:  ghc-filepath-devel
BuildRequires:  ghc-network-devel
BuildRequires:  ghc-process-devel
BuildRequires:  ghc-stm-devel
BuildRequires:  ghc-text-devel
BuildRequires:  ghc-time-devel
BuildRequires:  ghc-unix-devel
%if %{with ghc_prof}
BuildRequires:  ghc-base-prof
BuildRequires:  ghc-binary-prof
BuildRequires:  ghc-bytestring-prof
BuildRequires:  ghc-containers-prof
BuildRequires:  ghc-directory-prof
BuildRequires:  ghc-filepath-prof
BuildRequires:  ghc-network-prof
BuildRequires:  ghc-process-prof
BuildRequires:  ghc-stm-prof
BuildRequires:  ghc-text-prof
BuildRequires:  ghc-time-prof
BuildRequires:  ghc-unix-prof
%endif
BuildRequires:  pkgconfig(pangocairo)
BuildRequires:  pkgconfig(pango)
BuildRequires:  pkgconfig(cairo)
BuildRequires:  pkgconfig(gdk-pixbuf-2.0)
BuildRequires:  pkgconfig(libpulse)

# ~/.config/waymobar/waymobar.hs is compiled at run time with ghc, against
# the library (like xmobar).
Requires:       ghc-%{name}-devel%{?_isa} = %{version}-%{release}
Recommends:     rivermonad
Recommends:     libnotify

%description
waymobar is a status bar for Wayland compositors with wlr-layer-shell, in
the spirit of xmobar: configured with a Haskell file, with native widgets
for rivermonad (workspaces, layout, window title) read from its IPC
socket. The Wayland protocol is implemented in pure Haskell; text is drawn
with cairo and pango.


%package -n ghc-%{name}
Summary:        Haskell %{name} library

%description -n ghc-%{name}
This package provides the Haskell %{name} shared library.


%package -n ghc-%{name}-devel
Summary:        Haskell %{name} library development files
Provides:       ghc-%{name}-static = %{version}-%{release}
Provides:       ghc-%{name}-static%{?_isa} = %{version}-%{release}
%if %{defined ghc_version}
Requires:       ghc-compiler = %{ghc_version}
%endif
Requires:       ghc-%{name}%{?_isa} = %{version}-%{release}
Requires:       ghc-network-devel%{?_isa}
Requires:       pkgconfig(pangocairo)
Requires:       pkgconfig(pango)
Requires:       pkgconfig(cairo)
Requires:       pkgconfig(gdk-pixbuf-2.0)
Requires:       pkgconfig(libpulse)

%description -n ghc-%{name}-devel
This package provides the Haskell %{name} library development files,
needed to compile the waymobar configuration.


%if %{with haddock}
%package -n ghc-%{name}-doc
Summary:        Haskell %{name} library documentation
BuildArch:      noarch
Requires:       ghc-filesystem

%description -n ghc-%{name}-doc
This package provides the Haskell %{name} library documentation.
%endif


%if %{with ghc_prof}
%package -n ghc-%{name}-prof
Summary:        Haskell %{name} profiling library
Requires:       ghc-%{name}-devel%{?_isa} = %{version}-%{release}
Supplements:    (ghc-%{name}-devel and ghc-prof)

%description -n ghc-%{name}-prof
This package provides the Haskell %{name} profiling library.
%endif


%prep
%autosetup -n %{pkgver}


%build
%ghc_lib_build


%install
%ghc_lib_install


%files
%license LICENSE
%doc README.md
%{_bindir}/%{name}
%{_datadir}/%{pkgver}/


%files -n ghc-%{name} -f ghc-%{name}.files
%license LICENSE


%files -n ghc-%{name}-devel -f ghc-%{name}-devel.files


%if %{with haddock}
%files -n ghc-%{name}-doc -f ghc-%{name}-doc.files
%license LICENSE
%endif


%if %{with ghc_prof}
%files -n ghc-%{name}-prof -f ghc-%{name}-prof.files
%endif


%changelog
* Mon Nov 09 2026 Carlos Prieto Ortiz <prietus@live.com> - 0.1.0-1
- First release
