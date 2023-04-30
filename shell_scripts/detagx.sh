for tag in [FfCch] pdf html shtml doc docx ods xls ppt pptx bib tar tgz cpio
do
  echo find . -name *.$tag -exec chmod a-x {} \;
  find . -name *.$tag -exec chmod a-x {} \;
done
