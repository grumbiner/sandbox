<?php #test.php
$mainpage = "http://www.grumbinescience.org";
$thisdir = ".";
?>

<a href="<?php $mainpage?>/radix/">Radix</a><br/>
<a href="<?php $mainpage?>/radix/climate">Climate Articles</a><br/>

<hr/>

# directory tries:

<?php
echo "hello<br/>";
$files1 =  scandir($thisdir);
$numfiles = count($files1);

for ($i = 0; $i < $numfiles; $i++) {
  echo $files1[$i],"<br/>\n";
}
echo "<hr/>";
#---------------------------------------------------

echo "Reading read a file\n<br/>";
$pattern="/ /";
$files2 = file('a');

$numfiles = count($files2);
for ($i = 0; $i < $numfiles; $i++) {
  $terms = preg_split($pattern,$files2[$i]);
  $numterms = count($terms);
  echo '<a href='; echo "$terms[0]"; echo '>';
  for ($j = 1; $j < $numterms; $j++) {
    echo "$terms[$j] ";
  }
  echo "</a><br/>";
}

echo "<hr/>";

?>
