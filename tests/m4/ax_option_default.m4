#
# AX_OPTION_DEFAULT(type=[short|long],[autoconf_var])
#
# Convenience macro to decide whether to enable or disable particular
# test based on various runtime options.

AC_DEFUN([AX_OPTION_DEFAULT],[

   eval $1                     

   if test x$enable_all = xno ; then
      AS_TR_SH([$2])=no
   else
      if test x$type = xshort ; then
         AS_TR_SH([$2])=$3
      else
          if test x$enable_long = xyes ; then
             AS_TR_SH([$2])=$3
          else          
             AS_TR_SH([$2])=no     
          fi
      fi      
   fi

])

