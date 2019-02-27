
scrollex()
/* Section for stuff on 'Bit Transfer Operations'  */
/* an example of using scrollrect to move an image back and forth*/
/* speed depends on the area being redrawn	=  dh*dv			 */
{
	int 		 i, j;
	Rect		 trect;

	trect.top	 = 55;
	trect.bottom = 365;
	trect.left	 = 50;
	trect.right  = 450;

	for (j = 1; j< 11; j++)
	{
		for (i = 1; i< 11; i++)
		{
			ScrollRect(&trect, 10,0,alpha);
		}
		for (i = 1; i< 11; i++)
		{
			ScrollRect(&trect,-10,0,alpha);
		}
	}
	/* CopyBits example not given.*/

}
