# R-hub Build
rhub::list_validated_emails()
rhub::validate_email()

rhub::platforms()
rhub::platforms()$name

#  Debian Linux, R-devel, clang, ISO-8859-15 locale
rhub::check(platform = "debian-clang-devel")  
#  Debian Linux, R-devel, GCC
rhub::check(platform = "debian-gcc-devel")
#  Debian Linux, R-release, GCC
rhub::check(platform = " debian-gcc-release")
#  Debian Linux, R-devel, GCC ASAN/UBSAN
rhub::check(platform = "linux-x86_64-rocker-gcc-san")
#  macOS 10.11 El Capitan, R-release (experimental)
rhub::check(platform = "mac macos-elcapitan-release")
#  Windows Server 2008 R2 SP1, R-devel, 32/64 bit
rhub::check(platform = "windows-x86_64-devel")
# Windows Server 2012, R-devel, Rtools4.0, 32/64 bit (experimental)
rhub::check(platform = "windows-x86_64-devel-rtools4") 
#  Windows Server 2008 R2 SP1, R-release, 32/64 bit
rhub::check(platform = "windows-x86_64-release") 

