<!DOCTYPE html>
<?php
  $fullpath=getcwd ();
  $p=strpos($fullpath,"htdocs");
  $path=substr($fullpath,$p+6);
  $title = "Files in $path";
  $config = "allow.cfg";
  $no_found = "No Files Found";
?>
<head>
<title><?php echo $title; ?></title>
</head>
<body>

<h2><?php echo $title; ?></h2>
<?php
$myfile = fopen($config, "r") or die("$no_found");

$count = 1;
while(!feof($myfile)) {
   $line = trim(fgets($myfile));
   $line = preg_replace("/[^a-zA-Z0-9\*\.\_\-]/", "", $line);  //Only allow letters, numbers, *._-
   if (validate($line)) {
      foreach (glob($line) as $filename) {
         if ( "$filename" == "$config" ) continue;
         if ( $count == 1 ) { 
            echo "<table summary=\"$title\"><tr><th><u>Name</u></th><th><u>Date</u></th></tr>"; 
            $count++; 
         }
         if (is_dir($filename)) {
            //Test to see if directory has index file.
            if (file_exists($filename."/index.html") || file_exists($filename."/index.php") || 
                file_exists($filename."/index.shtml")) {
               show_item($filename,'d');
            }
         }
         else {
            show_item($filename,'f');
         }
      }
   }
}
if ( $count > 1 ) { 
  echo '</table>'; 
}
else {
  echo "$no_found";
}
fclose($myfile);

function show_item ($item, $type) {
   echo '<tr><td>';
   if ($type == 'd') {
      echo '<img src="/ncep_common/dir.gif" alt="">';
   }
   else {
      echo '<img src="/ncep_common/generic.gif" alt="">';
   }
   echo '&nbsp;<a href="'.$item.'">'.$item.'</a></td><td align="right">'.
         date ("d-M-Y H:i", filemtime($item)).'</td></tr>' ;
}

function validate($arg) {
  if (strlen($arg) == 0) return false;
  if ($arg == "*") return false;
  if (substr($arg,0,1) == ".") return false;
  if (substr_count($arg, '*') > 1) return false;
  return true;
}

?>
</body>
</html>
