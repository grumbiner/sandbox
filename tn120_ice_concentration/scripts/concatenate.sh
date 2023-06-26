for yy in 95 96 97 98 99 
do
    cat npsg.${yy}?? >> npsg.19${yy}
    cat spsg.${yy}?? >> spsg.19${yy}
done

for yy in 00 01 02 03 04 05
do
    cat npsg.20${yy}?? >> npsg.20${yy}
    cat spsg.20${yy}?? >> spsg.20${yy}
done
