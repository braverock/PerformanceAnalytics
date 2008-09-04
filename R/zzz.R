.onLoad <- function(lib, pkg)
{   
    # Startup Mesage and Desription:
    MSG <- if(getRversion() >= "2.5") packageStartupMessage else message
    dsc <- packageDescription(pkg)
    if(interactive() || getOption("verbose")) { 
        # not in test scripts
        MSG(paste("\nPackage ", pkg, " (",dsc$Version,") loaded.\n",
            dsc$Title, "\n", dsc$Copyright, " ", dsc$Author, ". License: ", dsc$License, "\n", dsc$URL,
            "\n", sep=""))
    }

}

