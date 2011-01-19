Name: openacd
Version: 0.9.5
Summary: OpenACD Call Center
Release: %{?buildno:%buildno}%{!?buildno:1}
Group: Applications/Communications
Vendor: Fused Solutions Inc.
Packager: SIPfoundry
License: GPL
AutoReqProv: no
URL: http://github.com/Vagabond/OpenACD/wiki
Source: %{name}-%{version}.tar.gz

# openssh for ssh-keygen command
BuildRequires: openssh

BuildRequires: automake
BuildRequires: gcc-c++
BuildRequires: erlang

# unclear how to express >= R13B04 or >= R13B-04 ?
Requires: erlang

BuildRoot: %{_builddir}/%{name}-root

%description
OpenACD is a skills-based, Call Center software based on FreeSWITCH and built in erlang.

%prep
%setup -n OpenACD

%build
make compile GIT_UPDATE_DISABLED=1

%install
make install PREFIX=$RPM_BUILD_ROOT/opt/OpenACD GIT_UPDATE_DISABLED=1

%clean
rm -rf $RPM_BUILD_ROOT

%pre

%post

%files
%dir /opt/OpenACD
%dir /opt/OpenACD/bin
%attr (755,root,root) /opt/OpenACD/bin/openacd
/opt/OpenACD/erts-*
/opt/OpenACD/etc
/opt/OpenACD/lib
/opt/OpenACD/log
/opt/OpenACD/releases
/opt/OpenACD/run
