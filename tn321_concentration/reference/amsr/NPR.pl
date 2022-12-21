#  This is the driver script for the NOAA Product Reformatter (NPR).  It (1) gets 
#  the PCF file from the current directory, (2) reads it, (3) parses the contents, 
#  (4) creates resource files for the program it needs to run, (5) runs that 
#  program, (6) manages the output, (7) does the high-level error handling, (8) 
#  creates a PSF file, and finally (9) exits returning its own return code value.
#
#  USAGE:
#      $PERL_LOC -w NPR.pl
#
#      $PERL_LOC  - The location of the perl interpreter
#
#  INPUTS: 
#      NPR.pl.PCF  - The NPR Process Control File.
#
#  OUTPUTS:
#      NPR.pl.PSF  - The NPR Process Status File.
#
#  Created by Tom King, June 3, 2009.
#             PSGS
#             Fairfax, VA
#             Thomas.S.King@noaa.gov
#
#  Modified by Chen Zhang, Dell, 05/12/2011
#             Chen.Zhang@noaa.gov
#             Added VIIRS_M and VIIRS_M3 BUFR
#
#  Modified by Chen Zhang, Dell, 05/24/2011
#             Chen.Zhang@noaa.gov
#             Added CrIS C1305 BUFR
#


   use FileHandle;
   use Fcntl qw(:DEFAULT :flock);
   use Sys::Syslog;
   use POSIX ":sys_wait_h";

#
#  Get the name of this program.
#

   $Name = `echo $0`;
   chomp($Name);
   @list = split('/',$Name);
   $Name = $list[-1];
   @list = split('\.',$Name);
   my $PROGRAM = $list[0];

   $log_file = "${PROGRAM}.pl.log";
   open (STDOUT, ">>$log_file");
   open (STDERR, ">&" . STDOUT);

   print "Starting at "; print `date`;

#
#  These are the PCF variables which need to be accessible to all scopes.
#

   my $OPS_BIN = "";
   my $PROD_TYPE = "";
   my $CONVERSION = "";
   my $CHAN_TYPE = "";
   my $BUFR_TABLE = "";
   my $NPR_INPUT = "";
   my $NPR_INPUT_FINAL = "";
   my $H5AUGJPSS = "";
   my $H5DUMP = "";

#
#  These are the PSF status variables which need to be accessible to all scopes.
#

   my $STATUS_NPR_PRODUCT = "";

#
#  This string will hold the output file name.
#

   my $NPR_PRODUCT = "";

   $rc = 0;
   $RET = 0;

#
#  Check for the presence of the PCF file.
#

   if( ! -e "NPR.pl.PCF" ) {
      print "Error in ${PROGRAM}.pl: NPR.pl.PCF does not exist. ";
      print "${PROGRAM}.pl will now exit.\n";
      $STATUS_NPR_PRODUCT = "NO";
      &make_psf();
      exit 1;
   }

#
#  Extract contents from the PCF file.
#

   sysopen(PCF, "NPR.pl.PCF",O_RDONLY) or $rc = 1;

   if( $rc != 0 ) {
      print "Error in ${PROGRAM}.pl: failed to open NPR.pl.PCF. ";
      print "${PROGRAM}.pl will now exit.\n";
      $STATUS_NPR_PRODUCT = "NO";
      &make_psf();
      exit 1;
   }

   @Line_List = <PCF>;

   close (PCF);

#
#  (1) Cycle through all the lines in the file, (2) get only those with an equal sign,
#  (3) split the line at the equal sign, (4) look for the PCF argument string, (5)
#  strip of any leading and trailing blank space from the string, and (6) assign to
#  a local variable.
#

   $Value = "";
   $Keyword_And_Argument = "";
   $Counter_Input = -1;

   foreach $line ( @Line_List ) {

      if( $line =~ m/=/ ) {

         chomp(($Keyword_And_Argument, $Value) = split('=', $line));

         if( $Keyword_And_Argument =~ m/OPS_BIN/ ) {
            $Value =~ s/^\s+//;
            $Value =~ s/\s+$//;
            $OPS_BIN = $Value;
         } elsif ( $Keyword_And_Argument =~ m/PROD_TYPE/ ) {
            $Value =~ s/^\s+//;
            $Value =~ s/\s+$//;
            $PROD_TYPE = $Value;
         } elsif ( $Keyword_And_Argument =~ m/CONVERSION/ ) {
            $Value =~ s/^\s+//;
            $Value =~ s/\s+$//;
            $CONVERSION = $Value;
         } elsif ( $Keyword_And_Argument =~ m/CHAN_TYPE/ ) {
            $Value =~ s/^\s+//;
            $Value =~ s/\s+$//;
            $CHAN_TYPE = $Value;
         } elsif ( $Keyword_And_Argument =~ m/BUFR_TABLE/ ) {
            $Value =~ s/^\s+//;
            $Value =~ s/\s+$//;
            $BUFR_TABLE = $Value;
         } elsif ( $Keyword_And_Argument =~ m/H5AUGJPSS/ ) {
            $Value =~ s/^\s+//;
            $Value =~ s/\s+$//;
            $H5AUGJPSS = $Value;
         } elsif ( $Keyword_And_Argument =~ m/H5DUMP/ ) {
            $Value =~ s/^\s+//;
            $Value =~ s/\s+$//;
            $H5DUMP = $Value;
         } elsif ( $Keyword_And_Argument =~ m/NPR_INPUT/ ) {
            $Value =~ s/^\s+//;
            $Value =~ s/\s+$//;
            $Counter_Input = $Counter_Input + 1;
            $NPR_INPUT[$Counter_Input] = $Value;
         }

      }

   }

#
#  check the Spacecraft_Maneuver and Operational_Mode in GEO. 
#  If they are "Normal Operations", then process them. Otherwise, exit.
   if ( $NPR_INPUT[0] =~ m/GATMO_npp/) {
   $AggregateBeginningDate =` $H5DUMP -a /Data_Products/ATMS-SDR-GEO/ATMS-SDR-GEO_Aggr/AggregateBeginningDate $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningDate);
   $AggregateBeginningDate=$parts[1];

   $AggregateBeginningTime =` $H5DUMP -a /Data_Products/ATMS-SDR-GEO/ATMS-SDR-GEO_Aggr/AggregateBeginningTime $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningTime);
   $AggregateBeginningTime=substr($parts[1],0,6).substr($parts[1],7,3);
   $AggregateBeginningTime=$AggregateBeginningDate.$AggregateBeginningTime;

   $ORBN =` $H5DUMP -a /Data_Products/ATMS-SDR-GEO/ATMS-SDR-GEO_Aggr/AggregateBeginningOrbitNumber $NPR_INPUT[0] | grep "(0,0):" `;
   chomp($ORBN);
   @parts=split(':', $ORBN);
   $ORBN=$parts[1];
   
   $Ascending=`$H5DUMP -d /Data_Products/ATMS-SDR-GEO/ATMS-SDR-GEO_Gran_0 -A $NPR_INPUT[0] | grep -p Ascending/Descending_Indicator | head -10 | grep "(0,0)" | awk '{print \$2}'`;
   chomp($Ascending);
   $AggregateBeginningTime=$AggregateBeginningTime.$Ascending;

   $Spacecraft_Maneuver=` $H5DUMP -a /Data_Products/ATMS-SDR-GEO/ATMS-SDR-GEO_Gran_0/N_Spacecraft_Maneuver $NPR_INPUT[0] | grep "(0,0):" `;
   $Operational_Mode=` $H5DUMP -a /Data_Products/ATMS-SDR-GEO/Operational_Mode $NPR_INPUT[0] | grep "(0,0):" `;
   if (($Spacecraft_Maneuver !~ m/Normal Operations/) || ($Operational_Mode !~ m/Normal Operations/)) {
         print "Error in granule data. It is Calibration Maneuver or Unknown\n";
         print "${PROGRAM}.pl will now exit.\n";
         $STATUS_NPR_PRODUCT = "NO";
         &make_psf();
         exit 1;       }
   }
   elsif ( $NPR_INPUT[0] =~ m/GMODO_npp/) {
   $AggregateBeginningDate =` $H5DUMP -a /Data_Products/VIIRS-MOD-GEO/VIIRS-MOD-GEO_Aggr/AggregateBeginningDate $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningDate);
   $AggregateBeginningDate=$parts[1];
   $AggregateBeginningTime =` $H5DUMP -a /Data_Products/VIIRS-MOD-GEO/VIIRS-MOD-GEO_Aggr/AggregateBeginningTime $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningTime);
   $AggregateBeginningTime=substr($parts[1],0,6).substr($parts[1],7,3);
   $AggregateBeginningTime=$AggregateBeginningDate.$AggregateBeginningTime;

   $ORBN =` $H5DUMP -a /Data_Products/VIIRS-MOD-GEO/VIIRS-MOD-GEO_Aggr/AggregateBeginningOrbitNumber $NPR_INPUT[0] | grep "(0,0):" `;
   chomp($ORBN);
   @parts=split(':', $ORBN);
   $ORBN=$parts[1];

   $Ascending=`$H5DUMP -d /Data_Products/VIIRS-MOD-GEO/VIIRS-MOD-GEO_Gran_0 -A $NPR_INPUT[0] | grep -p Ascending/Descending_Indicator | head -10 | grep "(0,0)" | awk '{print \$2}'`;
   chomp($Ascending);

   $AggregateBeginningTime=$AggregateBeginningTime.$Ascending;

   $Spacecraft_Maneuver=` $H5DUMP -a /Data_Products/VIIRS-MOD-GEO/VIIRS-MOD-GEO_Gran_0/N_Spacecraft_Maneuver $NPR_INPUT[0] | grep "(0,0):" `;
   $Operational_Mode=` $H5DUMP -a /Data_Products/VIIRS-MOD-GEO/Operational_Mode $NPR_INPUT[0] | grep "(0,0):" `;
   if (($Spacecraft_Maneuver !~ m/Normal Operations/) || ($Operational_Mode !~ m/Normal Operations/)) {
         print "Error in granule data. It is Calibration Maneuver or Unknown\n";
         print "${PROGRAM}.pl will now exit.\n";
         $STATUS_NPR_PRODUCT = "NO";
         &make_psf();
         exit 1;
                      }
   }
   elsif ( $NPR_INPUT[0] =~ m/GIMGO_npp/) {
   $AggregateBeginningDate =` $H5DUMP -a /Data_Products/VIIRS-IMG-GEO/VIIRS-IMG-GEO_Aggr/AggregateBeginningDate $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningDate);
   $AggregateBeginningDate=$parts[1];
   $AggregateBeginningTime =` $H5DUMP -a /Data_Products/VIIRS-IMG-GEO/VIIRS-IMG-GEO_Aggr/AggregateBeginningTime $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningTime);
   $AggregateBeginningTime=substr($parts[1],0,6).substr($parts[1],7,3);
   $AggregateBeginningTime=$AggregateBeginningDate.$AggregateBeginningTime;

   $ORBN =` $H5DUMP -a /Data_Products/VIIRS-IMG-GEO/VIIRS-IMG-GEO_Aggr/AggregateBeginningOrbitNumber $NPR_INPUT[0] | grep "(0,0):" `;
   chomp($ORBN);
   @parts=split(':', $ORBN);
   $ORBN=$parts[1];

   $Ascending=`$H5DUMP -d /Data_Products/VIIRS-IMG-GEO/VIIRS-IMG-GEO_Gran_0 -A $NPR_INPUT[0] | grep -p Ascending/Descending_Indicator | head -10 | grep "(0,0)" | awk '{print \$2}'`;
   chomp($Ascending);

   $AggregateBeginningTime=$AggregateBeginningTime.$Ascending;

   $Spacecraft_Maneuver=` $H5DUMP -a /Data_Products/VIIRS-IMG-GEO/VIIRS-IMG-GEO_Gran_0/N_Spacecraft_Maneuver $NPR_INPUT[0] | grep "(0,0):" `;
   $Operational_Mode=` $H5DUMP -a /Data_Products/VIIRS-IMG-GEO/Operational_Mode $NPR_INPUT[0] | grep "(0,0):" `;
   if (($Spacecraft_Maneuver !~ m/Normal Operations/) || ($Operational_Mode !~ m/Normal Operations/)) {
         print "Error in granule data. It is Calibration Maneuver or Unknown\n";
         print "${PROGRAM}.pl will now exit.\n";
         $STATUS_NPR_PRODUCT = "NO";
         &make_psf();
         exit 1;       
                      }
   }
   elsif ( $NPR_INPUT[0] =~ m/GMTCO_npp/) {
   $AggregateBeginningDate =` $H5DUMP -a /Data_Products/VIIRS-MOD-GEO-TC/VIIRS-MOD-GEO-TC_Aggr/AggregateBeginningDate $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningDate);
   $AggregateBeginningDate=$parts[1];
   $AggregateBeginningTime =` $H5DUMP -a /Data_Products/VIIRS-MOD-GEO-TC/VIIRS-MOD-GEO-TC_Aggr/AggregateBeginningTime $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningTime);
   $AggregateBeginningTime=substr($parts[1],0,6).substr($parts[1],7,3);
   $AggregateBeginningTime=$AggregateBeginningDate.$AggregateBeginningTime;

   $ORBN =` $H5DUMP -a /Data_Products/VIIRS-MOD-GEO-TC/VIIRS-MOD-GEO-TC_Aggr/AggregateBeginningOrbitNumber $NPR_INPUT[0] | grep "(0,0):" `;
   chomp($ORBN);
   @parts=split(':', $ORBN);
   $ORBN=$parts[1];

   $Ascending=`$H5DUMP -d /Data_Products/VIIRS-MOD-GEO-TC/VIIRS-MOD-GEO-TC_Gran_0 -A $NPR_INPUT[0] | grep -p Ascending/Descending_Indicator | head -10 | grep "(0,0)" | awk '{print \$2}'`;
   chomp($Ascending);

   $AggregateBeginningTime=$AggregateBeginningTime.$Ascending;

   $Spacecraft_Maneuver=` $H5DUMP -a /Data_Products/VIIRS-MOD-GEO-TC/VIIRS-MOD-GEO-TC_Gran_0/N_Spacecraft_Maneuver $NPR_INPUT[0] | grep "(0,0):" `;
   $Operational_Mode=` $H5DUMP -a /Data_Products/VIIRS-MOD-GEO-TC/Operational_Mode $NPR_INPUT[0] | grep "(0,0):" `;
   if (($Spacecraft_Maneuver !~ m/Normal Operations/) || ($Operational_Mode !~ m/Normal Operations/)) {
         print "Error in granule data. It is Calibration Maneuver or Unknown\n";
         print "${PROGRAM}.pl will now exit.\n";
         $STATUS_NPR_PRODUCT = "NO";
         &make_psf();
         exit 1;
                      }
   }
   elsif ( $NPR_INPUT[0] =~ m/GOTCO_npp/) {
   $AggregateBeginningDate =` $H5DUMP -a /Data_Products/OMPS-TC-GEO/OMPS-TC-GEO_Aggr/AggregateBeginningDate $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningDate);
   $AggregateBeginningDate=$parts[1];
   $AggregateBeginningTime =` $H5DUMP -a /Data_Products/OMPS-TC-GEO/OMPS-TC-GEO_Aggr/AggregateBeginningTime $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningTime);
   $AggregateBeginningTime=substr($parts[1],0,6).substr($parts[1],7,3);
   $AggregateBeginningTime=$AggregateBeginningDate.$AggregateBeginningTime;

   $ORBN =` $H5DUMP -a /Data_Products/OMPS-TC-GEO/OMPS-TC-GEO_Aggr/AggregateBeginningOrbitNumber $NPR_INPUT[0] | grep "(0,0):" `;
   chomp($ORBN);
   @parts=split(':', $ORBN);
   $ORBN=$parts[1];

   $Ascending=`$H5DUMP -d /Data_Products/OMPS-TC-GEO/OMPS-TC-GEO_Gran_0 -A $NPR_INPUT[0] | grep -p Ascending/Descending_Indicator | head -10 | grep "(0,0)" | awk '{print \$2}'`;
   chomp($Ascending);

   $AggregateBeginningTime=$AggregateBeginningTime.$Ascending;

   $Spacecraft_Maneuver=` $H5DUMP -a /Data_Products/OMPS-TC-GEO/OMPS-TC-GEO_Gran_0/N_Spacecraft_Maneuver $NPR_INPUT[0] | grep "(0,0):" `;
   $Operational_Mode=` $H5DUMP -a /Data_Products/OMPS-TC-GEO/Operational_Mode $NPR_INPUT[0] | grep "(0,0):" `;
   if (($Spacecraft_Maneuver !~ m/Normal Operations/) || ($Operational_Mode !~ m/Normal Operations/)) {
         print "Error in granule data. It is Calibration Maneuver or Unknown\n";
         print "${PROGRAM}.pl will now exit.\n";
         $STATUS_NPR_PRODUCT = "NO";
         &make_psf();
         exit 1;
                      }
   }
   elsif ( $NPR_INPUT[0] =~ m/GONPO_npp/) {
   $AggregateBeginningDate =` $H5DUMP -a /Data_Products/OMPS-NP-GEO/OMPS-NP-GEO_Aggr/AggregateBeginningDate $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningDate);
   $AggregateBeginningDate=$parts[1];
   $AggregateBeginningTime =` $H5DUMP -a /Data_Products/OMPS-NP-GEO/OMPS-NP-GEO_Aggr/AggregateBeginningTime $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningTime);
   $AggregateBeginningTime=substr($parts[1],0,6).substr($parts[1],7,3);
   $AggregateBeginningTime=$AggregateBeginningDate.$AggregateBeginningTime;

   $ORBN =` $H5DUMP -a /Data_Products/OMPS-NP-GEO/OMPS-NP-GEO_Aggr/AggregateBeginningOrbitNumber $NPR_INPUT[0] | grep "(0,0):" `;
   chomp($ORBN);
   @parts=split(':', $ORBN);
   $ORBN=$parts[1];

   $Ascending=`$H5DUMP -d /Data_Products/OMPS-NP-GEO/OMPS-NP-GEO_Gran_0 -A $NPR_INPUT[0] | grep -p Ascending/Descending_Indicator | head -10 | grep "(0,0)" | awk '{print \$2}'`;
   chomp($Ascending);

   $AggregateBeginningTime=$AggregateBeginningTime.$Ascending;

   $Spacecraft_Maneuver=` $H5DUMP -a /Data_Products/OMPS-NP-GEO/OMPS-NP-GEO_Gran_0/N_Spacecraft_Maneuver $NPR_INPUT[0] | grep "(0,0):" `;
   $Operational_Mode=` $H5DUMP -a /Data_Products/OMPS-NP-GEO/Operational_Mode $NPR_INPUT[0] | grep "(0,0):" `;
   if (($Spacecraft_Maneuver !~ m/Normal Operations/) || ($Operational_Mode !~ m/Normal Operations/)) {
         print "Error in granule data. It is Calibration Maneuver or Unknown\n";
         print "${PROGRAM}.pl will now exit.\n";
         $STATUS_NPR_PRODUCT = "NO";
         &make_psf();
         exit 1;
                      }
   }
   elsif ( $NPR_INPUT[0] =~ m/GW1AM2/) {
   $AggregateBeginningDate =` $H5DUMP -a /ObservationStartDateTime $NPR_INPUT[0] | grep "(0):" `;
   @parts=split('"', $AggregateBeginningDate);
   $SDATE=$parts[1];
   $SYYYY=substr($SDATE, 0,4);
   $SMM=substr($SDATE, 5,2);
   $SDD=substr($SDATE, 8,2);
   $SHH=substr($SDATE, 11,2);
   $SMIN=substr($SDATE, 14,2);
   $SSEC=substr($SDATE, 17,2).substr($SDATE, 20,1);
   $SMSEC=substr($SDATE, 17,2).substr($SDATE, 20,3);
   $AggregateBeginningTime="$SYYYY$SMM$SDD$SHH$SMIN$SMSEC";

   $AggregateEndDate =` $H5DUMP -a /ObservationEndDateTime $NPR_INPUT[0] | grep "(0):" `;
   @parts=split('"', $AggregateEndDate);
   $EDATE=$parts[1];
   $EYYYY=substr($EDATE, 0,4);
   $EMM=substr($EDATE, 5,2);
   $EDD=substr($EDATE, 8,2);
   $EHH=substr($EDATE, 11,2);
   $EMIN=substr($EDATE, 14,2);
   $ESEC=substr($EDATE, 17,2).substr($EDATE, 20,1);

   $ORBN =` $H5DUMP -a /StartOrbitNumber $NPR_INPUT[0] | grep "(0):" `;
   chomp($ORBN);
   @parts=split('"', $ORBN);
   $ORBN=$parts[1];
   }
   elsif ( $NPR_INPUT[0] =~ m/GAERO_npp/) {
   $AggregateBeginningDate =` $H5DUMP -a /Data_Products/VIIRS-Aeros-EDR-GEO/VIIRS-Aeros-EDR-GEO_Aggr/AggregateBeginningDate $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningDate);
   $AggregateBeginningDate=$parts[1];
   $AggregateBeginningTime =` $H5DUMP -a /Data_Products/VIIRS-Aeros-EDR-GEO/VIIRS-Aeros-EDR-GEO_Aggr/AggregateBeginningTime $NPR_INPUT[0] | grep "(0,0):" `;
   @parts=split('"', $AggregateBeginningTime);
   $AggregateBeginningTime=substr($parts[1],0,6).substr($parts[1],7,3);
   $AggregateBeginningTime=$AggregateBeginningDate.$AggregateBeginningTime;

   $ORBN =` $H5DUMP -a /Data_Products/VIIRS-Aeros-EDR-GEO/VIIRS-Aeros-EDR-GEO_Aggr/AggregateBeginningOrbitNumber $NPR_INPUT[0] | grep "(0,0):" `;
   chomp($ORBN);
   @parts=split(':', $ORBN);
   $ORBN=$parts[1];

   $Ascending=`$H5DUMP -d /Data_Products/VIIRS-Aeros-EDR-GEO/VIIRS-Aeros-EDR-GEO_Gran_0 -A $NPR_INPUT[0] | grep -p Ascending/Descending_Indicator | head -10 | grep "(0,0)" | awk '{print \$2}'`;
   chomp($Ascending);

   $AggregateBeginningTime=$AggregateBeginningTime.$Ascending;

   $Spacecraft_Maneuver=` $H5DUMP -a /Data_Products/VIIRS-Aeros-EDR-GEO/VIIRS-Aeros-EDR-GEO_Gran_0/N_Spacecraft_Maneuver $NPR_INPUT[0] | grep "(0,0):" `;
   $Operational_Mode=` $H5DUMP -a /Data_Products/VIIRS-Aeros-EDR-GEO/Operational_Mode $NPR_INPUT[0] | grep "(0,0):" `;
   if (($Spacecraft_Maneuver !~ m/Normal Operations/) || ($Operational_Mode !~ m/Normal Operations/)) {
         print "Error in granule data. It is Calibration Maneuver or Unknown\n";
         print "${PROGRAM}.pl will now exit.\n";
         $STATUS_NPR_PRODUCT = "NO";
         &make_psf();
         exit 1;
                      }
   }

#
#  Check for the existence of the input files and transfer it.
#

   $Counter_Input=-1;
   $start_time=time();
   foreach $input_file (@NPR_INPUT) {
      if( ! -e "$input_file" ) {
         print "Error in ${PROGRAM}.pl: an NPR_INPUT file $input_file does NOT exist. ";
         print "${PROGRAM}.pl will now exit.\n";
         $STATUS_NPR_PRODUCT = "NO";
         &make_psf();
         exit 1;
      }
   if ( $input_file=~ m/NUCAPS/) {
   $Counter_Input=$Counter_Input + 1;
   $NPR_INPUT_FINAL[$Counter_Input]=$input_file; 
      } 
   elsif ( $input_file=~ m/ACSPO/) {
   $Counter_Input=$Counter_Input + 1;
   $NPR_INPUT_FINAL[$Counter_Input]=$input_file; 
      }
   elsif ( $input_file=~ m/AMV/) {
   $Counter_Input=$Counter_Input + 1;
   $NPR_INPUT_FINAL[$Counter_Input]=$input_file; 
      }
   elsif ( $input_file=~ m/GVF-WKL/) {
   $Counter_Input=$Counter_Input + 1;
   $NPR_INPUT_FINAL[$Counter_Input]=$input_file; 
      }
   elsif ( $input_file=~ m/GW1AM2/) {
   $Counter_Input=$Counter_Input + 1;
   $NPR_INPUT_FINAL[$Counter_Input]=$input_file; 
      }
   else {
   @parts=split('/', $input_file);
   $nc_name=$parts[-1];
   $nc_name=~ s/h5/nc/;
   $rc=system("cp $input_file $nc_name");
   if( $rc != 0 ) {
      print "Error in ${PROGRAM}.pl: the copy data returned an error. ";
      print "${PROGRAM}.pl will now exit.\n";
      $STATUS_NPR_PRODUCT = "NO";
      &make_psf();
      exit 1;
                   }

         if( ! -e "$H5AUGJPSS" ) {
            print "Error in ${PROGRAM}.pl: the program H5AUGJPSS $H5AUGJPSS does
 NOT exist. ";
            print "${PROGRAM}.pl will now exit.\n";
            $STATUS_NPR_PRODUCT = "NO";
            &make_psf();
            exit 1;
         }

   $rc=system("$H5AUGJPSS -o1 -o4 $nc_name");
   if( $rc != 0 ) {
      print "Error in ${PROGRAM}.pl: $H5AUGJPSS returned an error. ";
      print "${PROGRAM}.pl will now exit.\n";
      $STATUS_NPR_PRODUCT = "NO";
      &make_psf();
      exit 1;
                    }

         if( ! -e "$H5DUMP" ) {
            print "Error in ${PROGRAM}.pl: the program H5DUMP $H5DUMP does
 NOT exist. ";
            print "${PROGRAM}.pl will now exit.\n";
            $STATUS_NPR_PRODUCT = "NO";
            &make_psf();
            exit 1;
         }

   $Counter_Input=$Counter_Input + 1;
   $NPR_INPUT_FINAL[$Counter_Input]=$nc_name; 
   }
 
   }
   $end_time=time();

$h5_time=$end_time - $start_time;

#
#  Create the output file name strings from the input file name strings.
#
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=gmtime(time);
$year=$year+1900;
$year = sprintf("%04d", $year);
$mon=$mon+1;
$mon = sprintf("%02d", $mon);
$mday = sprintf("%02d", $mday);
$hour = sprintf("%02d", $hour);
$min = sprintf("%02d", $min);
$sec = sprintf("%02d", $sec);

   if ( $NPR_INPUT[0] =~ m/NUCAPS_ALL/ && $PROD_TYPE =~ m/NUCAPS/ ) {
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT=$List[-1];
      # Remove the ".nc" 
      @List = split('\.',$NPR_PRODUCT);
      $NPR_PRODUCT=$List[0];
      # CrIS C1305 BUFR
      @List = split('_',$NPR_PRODUCT);
      $NPR_PRODUCT = "NUCAPS-C1305_v1r0_npp_s$List[2]$List[3]_e$List[2]$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";

   } elsif ( $NPR_INPUT[0] =~ m/NUCAPS/ && $PROD_TYPE =~ m/NUCAPS/ ) {
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT=$List[-1];
      # Remove the ".nc" 
      @List = split('\.',$NPR_PRODUCT);
      $NPR_PRODUCT=$List[0];
      # CrIS C0399 BUFR
      @List = split('_',$NPR_PRODUCT);
      $NPR_PRODUCT = "NUCAPS-C0399_v1r0_npp_s$List[3]$List[4]_e$List[3]$List[5]_c$year$mon$mday$hour$min$sec"."0.bufr";

   } elsif ( $NPR_INPUT[0] =~ m/ATM/ && $PROD_TYPE =~ m/ATMS/ ) {

      # ATMS BUFR
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT = $List[-1];
      @List = split('_',$NPR_PRODUCT);

      $List[2] = substr($List[2], 1);
      $List[3] = substr($List[3], 1);
      $List[4] = substr($List[4], 1);
#      $ORBN = substr($List[5], 1);
      $NPR_PRODUCT = "ATMS_v1r0_npp_s$List[2]$List[3]_e$List[2]$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";

   } elsif ( $NPR_INPUT[0] =~ m/GMOD/ && $PROD_TYPE =~ m/VIIRS_M/ ) {

      # VIIRS M-Band BUFR
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT = $List[-1];
      @List = split('_',$NPR_PRODUCT);

      $List[2] = substr($List[2], 1);
      $List[3] = substr($List[3], 1);
      $List[4] = substr($List[4], 1);
#      $ORBN = substr($List[5], 1);
      $NPR_PRODUCT = "ATMS_v1r0_npp_s$List[2]$List[3]_e$List[2]$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";

      if( $CHAN_TYPE =~ m/M4_CHAN/ ) {
      $NPR_PRODUCT = "VIIRSM4_v1r0_npp_s$List[2]$List[3]_e$List[2]$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";
      } else {
      $NPR_PRODUCT = "VIIRSM_v1r0_npp_s$List[2]$List[3]_e$List[2]$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";
      }
   } elsif ( $NPR_INPUT[0] =~ m/GIMGO/ && $PROD_TYPE =~ m/VIIRS_I/ ) {

      # VIIRS I-Band BUFR
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT = $List[-1];
      @List = split('_',$NPR_PRODUCT);

      $List[2] = substr($List[2], 1);
      $List[3] = substr($List[3], 1);
      $List[4] = substr($List[4], 1);
#      $ORBN = substr($List[5], 1);

      if( $CHAN_TYPE =~ m/I5_CHAN/ ) {
      $NPR_PRODUCT = "VIIRSI5_v1r0_npp_s$List[2]$List[3]_e$List[2]$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";
      } else {
      $NPR_PRODUCT = "VIIRSI_v1r0_npp_s$List[2]$List[3]_e$List[2]$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";
      }

   } elsif ( $NPR_INPUT[0] =~ m/GMTCO/ && $PROD_TYPE =~ m/SST/ ) {

      # VIIRS SST BUFR
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT = $List[-1];
      @List = split('_',$NPR_PRODUCT);

      $List[2] = substr($List[2], 1);
      $List[3] = substr($List[3], 1);
      $List[4] = substr($List[4], 1);
#      $ORBN = substr($List[5], 1);

      $NPR_PRODUCT = "SST_v1r0_npp_s$List[2]$List[3]_e$List[2]$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";
   } elsif ( $NPR_INPUT[0] =~ m/GOTCO/ && $PROD_TYPE =~ m/OMPS-TC/ ) {

      # VIIRS SST BUFR
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT = $List[-1];
      @List = split('_',$NPR_PRODUCT);

      $List[2] = substr($List[2], 1);
      $List[3] = substr($List[3], 1);
      $List[4] = substr($List[4], 1);
#      $ORBN = substr($List[5], 1);

      $NPR_PRODUCT = "OMPS-TC_v1r0_npp_s$List[2]$List[3]_e$List[2]$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";
   } elsif ( $NPR_INPUT[0] =~ m/GONPO/ && $PROD_TYPE =~ m/OMPS-NP/ ) {

      # VIIRS SST BUFR
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT = $List[-1];
      @List = split('_',$NPR_PRODUCT);

      $List[2] = substr($List[2], 1);
      $List[3] = substr($List[3], 1);
      $List[4] = substr($List[4], 1);
#      $ORBN = substr($List[5], 1);

      $NPR_PRODUCT = "OMPS-NP_v1r0_npp_s$List[2]$List[3]_e$List[2]$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";

   } elsif ( $NPR_INPUT[0] =~ m/ACSPO/ ) {
      # ACSPO SST BUFR
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT = $List[-1];
      @List = split('_',$NPR_PRODUCT);
      $NPR_PRODUCT = "ACSPO-SST_v1r0_npp_$List[3]_$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";
   } elsif ( $NPR_INPUT[0] =~ m/AMV/ ) {
      # VIIRS WINDS BUFR
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT = $List[-1];
      @List = split('_',$NPR_PRODUCT);
      $NPR_PRODUCT = "PAMV_v1r0_npp_$List[3]_$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";
   } elsif ( $NPR_INPUT[0] =~ m/GVF-WKL/ ) {
      # GVF-WKL-GLB and GVF-WKL-REG
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT = $List[-1];
      $STARTTIME=substr($NPR_PRODUCT,22, 8);
      $STARTTIME=$STARTTIME."000000";
      $StartPart=substr($NPR_PRODUCT,0, 40);
      $NPR_PRODUCT = $StartPart."_c$year$mon$mday$hour$min$sec"."0.grib2";
   } elsif ( $NPR_INPUT[0] =~ m/GAERO/ && $PROD_TYPE =~ m/AOT/ ) {
      # VIIRS AOT BUFR
      # Remove the path
      @List = split('/',$NPR_INPUT[0]);
      $NPR_PRODUCT = $List[-1];
      @List = split('_',$NPR_PRODUCT);

      $List[2] = substr($List[2], 1);
      $List[3] = substr($List[3], 1);
      $List[4] = substr($List[4], 1);
      $NPR_PRODUCT = "AOT_v1r0_npp_s$List[2]$List[3]_e$List[2]$List[4]_c$year$mon$mday$hour$min$sec"."0.bufr";

   } elsif ( $NPR_INPUT[0] =~ m/GW1AM2/) {
      # AMSR2 1B BUFR
      # Remove the path
      $NPR_PRODUCT = "AMSR2-MBT-LR_v1r0_GW1_s$SYYYY$SMM$SDD$SHH$SMIN$SSEC"."_e$EYYYY$EMM$EDD$EHH$EMIN$ESEC"."_c$year$mon$mday$hour$min$sec"."0.bufr";
      $NPR_PRODUCT_A = "AMSR2-MBT-89A_v1r0_GW1_s$SYYYY$SMM$SDD$SHH$SMIN$SSEC"."_e$EYYYY$EMM$EDD$EHH$EMIN$ESEC"."_c$year$mon$mday$hour$min$sec"."0.bufr";
      $NPR_PRODUCT_B = "AMSR2-MBT-89B_v1r0_GW1_s$SYYYY$SMM$SDD$SHH$SMIN$SSEC"."_e$EYYYY$EMM$EDD$EHH$EMIN$ESEC"."_c$year$mon$mday$hour$min$sec"."0.bufr";

   } else {

      print "Error in ${PROGRAM}.pl: input file name has no product definition in this script. ";
      print "${PROGRAM}.pl will now exit.\n";
      $STATUS_NPR_PRODUCT = "NO";
      &make_psf();
      exit 1;

   }

#
#  Make the resource file
#

   open(RES, ">npr.filenames") or $rc = 1;

   if( $rc != 0 ) {
      print "Error in ${PROGRAM}.pl: failed to open npr.filenames. ";
      print "${PROGRAM}.pl will now exit.\n";
      $STATUS_NPR_PRODUCT = "NO";
      &make_psf();
      exit 1;
   }

   print RES "$PROD_TYPE\n";
   print RES "$CONVERSION\n";

   if(($PROD_TYPE =~ m/VIIRS_I/)||($PROD_TYPE =~ m/VIIRS_M/)) {
      print RES "$CHAN_TYPE\n";
   }

   foreach $input_file (@NPR_INPUT_FINAL) {
      print RES "$input_file\n";
   }

   print RES "$NPR_PRODUCT\n";
   if ( $NPR_INPUT[0] =~ /GW1AM2/) {
   print RES "$NPR_PRODUCT_A\n";
   print RES "$NPR_PRODUCT_B\n";
}

   if( length($BUFR_TABLE) > 0 ) {

      print RES "$BUFR_TABLE\n";

   }
   if (( $NPR_INPUT[0] =~ /NUCAPS/) || ( $NPR_INPUT[0] =~ /NPP_VIIRS/) || ($NPR_INPUT[0] =~ /ACSPO/)) {
   close (RES);
   }
   elsif ( $NPR_INPUT[0] =~ /GVF-WKL/) {
   print RES "$STARTTIME\n";
   close (RES);
   }
   else {
   print RES "$ORBN\n";
   print RES "$AggregateBeginningTime\n";
   close (RES);
   }

#
#  Check for the existence of the subsetter code.
#

   if( ! -e "$OPS_BIN/main_npr" ) {

      print "Error in ${PROGRAM}.pl: the program $OPS_BIN/main_npr does NOT exist. ";
      print "${PROGRAM}.pl will now exit.\n";
      $STATUS_NPR_PRODUCT = "NO";
      &make_psf();
      exit 1;

   }

#
#  Run the subsetter code.
#

   $rc = system("$OPS_BIN/main_npr");
$end_time=time();
$total_time=$end_time - $start_time;
print "Total_time=$total_time\n";

   if( $rc != 0 ) {
      print "Error in ${PROGRAM}.pl: the program main_npr returned an error. ";
      print "${PROGRAM}.pl will now exit.\n";
      $STATUS_NPR_PRODUCT = "NO";
      &make_psf();
      exit 1;

   }

#
#  Make the PSF file.
#

   &make_psf();

   print "Ending at "; print `date`;

   exit $RET;


   sub make_psf {

      my $rc = 0;

      open(PSF, ">NPR.pl.PSF") or $rc = 1;

      if( $rc != 0 ) {
         print "Error in ${PROGRAM}.pl: failed to open NPR.pl.PSF. ";
         print "No NPR.pl.PSF will be generated. ";
         print "${PROGRAM}.pl will now exit.\n";
         exit 1;
      }

      if( $STATUS_NPR_PRODUCT ne "NO" ) {
         print PSF "$NPR_PRODUCT\n";
      }

      close (PSF);

   }

