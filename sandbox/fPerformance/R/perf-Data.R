
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA


################################################################################
# FUNCTION:            DEMO DATA SET:
#  R                    Monthly portfolio returns
#  B                    Monthly benchmark returns
#  RB                   Merged portfolio and benchmark returns
################################################################################


    # Monthly Returns:

    # A data set implemented by Diethelm Wuertz

    .r = c(0.3,  2.6,  1.1, -1.0,  1.5,  2.5, 1.6, 6.7, -1.4, 4.0, -0.5,  8.1, 
           4.0, -3.7, -6.1,  1.7, -4.9, -2.2, 7.0, 5.8, -6.5, 2.4, -0.5, -0.9)
    .b = c(0.2,  2.5,  1.8, -1.1,  1.4,  1.8, 1.4, 6.5, -1.5, 4.2, -0.6,  8.3,
           3.9, -3.8, -6.2,  1.5, -4.8,  2.1, 6.0, 5.6, -6.7, 1.9, -0.3,  0.0)
       
       
# ------------------------------------------------------------------------------
   
      
    # As Signal Series:

    # A data set implemented by Diethelm Wuertz

    R = timeSeries(.r, units = "R")
    B = timeSeries(.b, units = "B")
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    

################################################################################

