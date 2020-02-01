# R-hub Build
rhub::list_validated_emails()
rhub::validate_email()

rhub::platforms()
rhub::platforms()$name

rhub::check(platform = rhub::platforms()$name[1]) # debian-clang-devel
rhub::check(platform = rhub::platforms()$name[2]) # debian-gcc-devel
rhub::check(platform = rhub::platforms()$name[5]) # debian-gcc-release
rhub::check(platform = rhub::platforms()$name[10]) # linux-x86_64-rocker-gcc-san  
rhub::check(platform = rhub::platforms()$name[11]) # mac macos-elcapitan-release
rhub::check(platform = rhub::platforms()$name[16]) # windows-x86_64-devel
rhub::check(platform = rhub::platforms()$name[20]) # windows-x86_64-release 

