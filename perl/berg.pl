#!/usr/bin/perl

while ($line = <STDIN>) {
  @words=split(/,/,$line);
  #print $words[6],"\n";
  if ($words[6] =~ /(\d{2})[xX](\d{2})/) {
    #print $1," ",$2,"\n";
    print $words[0], " ",$1*$2*(111/60)*(111/60),"\n";
  }
    
}

#"B22A",2007032,-74.24,"S",-107.51,"W","46X27","Envisat"
#"b15r",2007032,-67.21,"s",77.37,"E","22x05","MODIS"
#"B15J",2007032,-75.52,"S",168.03,"E","27X15","MODIS"
#"b15j",2007032,-75.55,"s",168.05,"E","30x22","Envisat"
#"b15a",2007032,-65.3,"s",156.07,"E","61x11","Envisat"
#"b15f",2007032,-65.01,"s",109.41,"E","21x08","Envisat"
#"b15b",2007032,-67.14,"s",75.36,"E","39x16","MODIS"
#"B15S",2007032,-76.03,"S",168.08,"E","10X02","MODIS"
#"B21A",2007032,-74.44,"S",-102.17,"W","13X08","Envisat"
#"b15l",2007031,-66.06,"s",49.3,"E","16x07","Envisat"
#"b15p",2007031,-62.37,"s",151.27,"E","10x02","MODIS"
#"b15g",2007031,-66.19,"s",47.12,"E","26x12","Envisat"
#"b15m",2007030,-65.43,"s",143.48,"E","29x03","Envisat"
#"b15k",2007030,-67.1,"s",147.28,"E","32x05","Envisat"
#"b09b",2007030,-67.12,"s",148.34,"E","51x19","Envisat"
#"b15n",2007030,-65.36,"s",140.15,"E","31x07","Envisat"
#"b15d",2007030,-69.06,"s",-56,"W","29x08","Envisat"
#"b15i",2007029,-66.59,"s",150.14,"E","11x02","Envisat"
#"b17b",2007029,-65.1,"s",129,"E","27x11","Envisat"
#"b16",2007029,-66.49,"s",149.59,"E","16x07","Envisat"
#"b17a",2007029,-67.28,"s",150.51,"E","19x08","Envisat"
#"b15q",2007029,-67.22,"s",151.11,"E","14x03","Envisat"
#"b09b",2007025,-67.12,"s",148.34,"E","51x19","Envisat"
#"b21a",2007025,-74.44,"s",-102.17,"W","13x08","Envisat"
#"b17b",2007025,-65.1,"s",129,"E","27x11","Envisat"
#"b17a",2007025,-67.27,"s",150.52,"E","19x08","Envisat"
#"b22a",2007025,-74.24,"s",-107.51,"W","46x27","Envisat"
#"b15q",2007025,-67.2,"s",151.12,"E","14x03","Envisat"
#"b16",2007025,-66.49,"s",149.59,"E","16x07","Envisat"
#"b15i",2007025,-66.59,"s",150.14,"E","11x02","Envisat"
#"b15a",2007025,-65.36,"s",155.58,"E","61x11","Envisat"
#"b15j",2007025,-75.52,"s",168.02,"E","30x22","Envisat"
#"b15b",2007024,-67.12,"s",75.43,"E","39x16","MODIS"
#"b15d",2007024,-69.18,"s",-56.02,"W","29x08","MODIS"
#"b15p",2007024,-62.56,"s",151.14,"E","10x02","MODIS"
#"b15r",2007024,-67.17,"s",77.27,"E","22x05","MODIS"
#"b15l",2007024,-65.39,"s",50.52,"E","16x07","MODIS"
#"b15k",2007023,-67.03,"s",147.48,"E","32x05","Envisat"
#"b15g",2007023,-66.16,"s",47.55,"E","26x12","Envisat"
#"b15n",2007023,-65.39,"s",141.1,"E","31x07","Envisat"
#"b15m",2007023,-65.47,"s",144.47,"E","29x03","Envisat"
#"b15f",2007020,-65.14,"s",109.58,"E","21x08","Envisat"
#"b15r",2007018,-67.13,"s",77.02,"E","22x05","Envisat"
#"b09b",2007018,-67.12,"s",148.34,"E","51x19","Envisat"
#"b15b",2007018,-67.1,"s",75.47,"E","39x16","Envisat"
#"b21a",2007018,-74.44,"s",-102.17,"W","13x08","Envisat"
#"b15i",2007018,-66.59,"s",150.14,"E","11x02","Envisat"
#"b17a",2007018,-67.28,"s",150.56,"E","19x08","Envisat"
#"b16",2007018,-66.49,"s",149.59,"E","16x07","Envisat"
#"b15j",2007018,-75.49,"s",168.17,"E","30x22","Envisat"
#"b15q",2007018,-67.24,"s",151.3,"E","14x03","Envisat"
#"b15m",2007018,-65.49,"s",145.35,"E","29x03","Envisat"
#"b22a",2007018,-74.24,"s",-107.51,"W","46x27","Envisat"
#"b15k",2007018,-66.43,"s",148.06,"E","32x05","Envisat"
#"b15n",2007018,-65.46,"s",141.5,"E","31x07","Envisat"
#"b15d",2007017,-69.4,"s",-56.12,"W","29x08","Envisat"
#"b15f",2007017,-65.17,"s",110.32,"E","21x08","Envisat"
#"b15g",2007016,-66.26,"s",48.26,"E","26x12","Envisat"
#"b15l",2007016,-65.28,"s",53.14,"E","16x07","Envisat"
#"b15a",2007016,-65.3,"s",156.1,"E","61x11","Envisat"
#"b17b",2007016,-65.14,"s",129.49,"E","27x11","Envisat"
#"b15d",2007011,-70.04,"s",-56.23,"W","29x08","Envisat"
#"b21a",2007011,-74.44,"s",-102.17,"W","13x08","Envisat"
#"b15n",2007011,-65.48,"s",142.23,"E","31x07","Envisat"
#"b15j",2007011,-75.46,"s",168.15,"E","30x22","Envisat"
#"b09b",2007011,-67.12,"s",148.34,"E","51x19","Envisat"
#"b15f",2007011,-65.22,"s",112.32,"E","21x08","Envisat"
#"b22a",2007011,-74.24,"s",-107.51,"W","46x27","Envisat"
#"b17b",2007010,-65.18,"s",129.54,"E","27x11","Envisat"
#"b17a",2007010,-67.27,"s",150.57,"E","19x08","Envisat"