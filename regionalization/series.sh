for name in Hudson Okhotsk Bering Baffin Labrador St_Law Baltic Azov Ob White Japan Yellow Black Maine Chesapeake Deleware
do
  grep $name nh_area.* > nh_$name
  cat nh_$name | cut -c41-61 > nh_$name.areas
  ./series nh_$name.areas > nh_$name.acor

  grep $name glob_area.* > glob_$name
  cat glob_$name | cut -c43-63 > glob_$name.areas
  ./series glob_$name.areas > glob_$name.acor
done
