/* DiskTimerII.c		Steve Brecher
 *
 * This is an MPW C source file.  Set Tabs to 4.
 *
 * This program does a performance test on the volume from which it is launched:
 *		Data transfer speed:
 *			24KB reads
 *			24KB writes
 *		Access time:
 *			1 512-byte read from start of test area followed by 1 512-byte read
 *	        from offset 1MB from start of test area  (volume must be at least
 *			test area offset + 1MB + 512bytes in size)
 *
 * Each test is performed multiple times.
 *
 * The write tests use the data that was previously read, so the test is
 * non-destructive.
 *
 * The grandfather program, DiskBench, did 32KB I/O starting at volume offset 0.
 * That was unfair if the volume started within 32KB of a cylinder boundary, or
 * if the first 32KB happened to contain a remapped block or track, or required
 * controller or driver retries due to a soft error.  The father program,
 * DiskTimer, did a 32KB I/O pre-calibration by doing a smaller number of I/O
 * iterations at progressive 4KB offsets up to 32KB from the start of the
 * volume, and using the offset which resulted in the best time for the
 * reported tests.  This program, DiskTimerII, improves the calibration
 * procedure by using progressive offsets of 512 bytes (one sector) and
 * extending the calibration range to 48KB.  Hence the chance that the "best"
 * location per the calibration will include a head step or a remapped area is
 * greatly reduced. The number of calibration I/Os at each location has been
 * reduced from 10 to 5 to keep the total clock time reasonable.  Further,
 * the I/O size is reduced to 24KB from 32KB so that  2-surface RLL drives
 * (12KB per track) will not have to step during each I/O.
 *
 * To a avoid constant sector skew between I/Os, which might be unfair
 * to some drives, there is a varying delay between I/O requests.  The
 * delay is obtained via a pseudo-random number generator, using the same
 * seed on each run.
 *
 * In order to avoid confusion with results from the previous DiskBench
 * and DiskTimer programs, DiskTimerII reports results in deciseconds rather
 * than in ticks or seconds.  A rotating beach ball cursor during the test
 * has been added to assure users that progress is being made.
 */
 
#define		Version	"1.0"	/* 1-Nov-86 */

#include	<Types.h>
#include	<Strings.h>
#include	<Resources.h>
#include	<Quickdraw.h>
#include	<Fonts.h>
#include	<Windows.h>
#include	<TextEdit.h>
#include	<Dialogs.h>
#include	<Memory.h>
#include	<Events.h>
#include	<Files.h>
#include	<SegLoad.h>

/* If the following two counts are changed, DITL 129 must be changed also */
#define	TransferCount	100	/* number of times to perform data transfer tests */
#define	SeekCount		80	/* number of times to perform seek test */

#define CalibCount		5	/* number of read iterations for calibration */
#define	XferSize		(24*1024)
#define CalibRange		(48*1024)
#define	WaitDlogID		128
#define	ResultsAlertID	129
#define	ErrorAlertID	130
#define	GreetAlertID	131
#define	StrVolTooSmall	128

#define	CurApRefNum		((short *)0x900)
#define	FCBsPtr			((Ptr *)0x34E)
#define	Ticks			((longword *)0x16A)
#define	fcbVPtr			20

typedef unsigned long	longword;

DialogPtr	DlogPtr;
Handle		BuffHndl;	/* to I/O buffer */
IOParam		ioPB;
longword	XferRdTicks, XferWtTicks, AccessTicks;
Boolean		SkipAccess;	/* flag to skip access tests if volume too small */
longword	VolOffset;
pascal void	SpinCursor(inc) short inc; extern;

void Error(err)
	OSErr	err;
	{
	char	ErrStr[8];

	DisposDialog(DlogPtr);
	sprintf(ErrStr, "%d", err);
	ParamText(ErrStr, 0, 0, 0);
	StopAlert(ErrorAlertID, 0);
	ExitToShell();
}

/*
 * Put up intro alert; get data buffer.  Return true/false for OK/Cancel.
 * If OK, put up "Please wait" dialog.
 * Note:  random seed is initted to 1 by InitGraf.
 */
Boolean Initialize()
	{
	extern struct qd qd;

    InitGraf(&qd.thePort); InitFonts(); InitWindows(); TEInit(); InitDialogs(0);
	FlushEvents(everyEvent, 0);
	
	BuffHndl = NewHandle(XferSize);
	InitCursor();
	if (Alert(GreetAlertID, 0L) == ok) {
		DlogPtr = GetNewDialog(WaitDlogID, 0, (WindowPtr)-1);
		BeginUpdate(DlogPtr);
		DrawDialog(DlogPtr);
		EndUpdate(DlogPtr);
		return true; }
	return false;
}


longword XferTest(Count, Write)
	int		Count;
	Boolean	Write;
	{
	int			i,j;
	OSErr		err;
	longword	StartTicks, PreDelayTicks, PostDelayTicks;

	StartTicks = *Ticks;
	for (i=0; i<Count; ) {
		ioPB.ioPosOffset = VolOffset;
		if (Write)
			err = PBWrite(&ioPB, false);
		else
			err = PBRead(&ioPB, false);
		if (err)
			Error(err);
		if (++i < Count) {
			j = 0;
			PreDelayTicks = *Ticks;
			PostDelayTicks = PreDelayTicks + (Random()&7)+1;
			while (*Ticks < PostDelayTicks)
				if (j++ == 0)
					SpinCursor(32);
			StartTicks += *Ticks - PreDelayTicks; } }
	return *Ticks - StartTicks;
}

/*
 * Set up ioPB with driver refNum and default volume's drive number.
 * Set value of SkipAccess flag.
 * Determine which offset in the set 0,0.5K,1.0K,...47.5K from start of volume
 * results in best read time.
 */
void Calibrate()
	{
	VCB			*vcbPtr;
	longword	TempTicks, BestTicks, BestOffset;

	vcbPtr = *(VCB **)(*FCBsPtr + *CurApRefNum + fcbVPtr);
	ioPB.ioRefNum = vcbPtr->vcbDRefNum;
	ioPB.ioVRefNum = vcbPtr->vcbDrvNum;
	ioPB.ioBuffer = *BuffHndl;
	ioPB.ioPosMode = fsFromStart;
	ioPB.ioReqCount = 512;
	VolOffset = 0;
	XferTest(1, false); /* seek to start of volume */
	ioPB.ioReqCount = XferSize;
	BestTicks = 0xFFFFFFFF;
	for (VolOffset=0; VolOffset<CalibRange; VolOffset+=512)
		if ((TempTicks=XferTest(CalibCount, false)) < BestTicks) {
			BestTicks = TempTicks;
			BestOffset = VolOffset; }
	VolOffset = BestOffset;
	ioPB.ioReqCount = 512;
	XferTest(1, false); /* seek to start of test area */
	ioPB.ioReqCount = XferSize;
	SkipAccess = (vcbPtr->vcbAlBlkSiz * vcbPtr->vcbNmAlBlks) < VolOffset + 1024*1024+512;
}

longword AccessTest()
	{
	int			i,j;
	OSErr		err;
	longword	StartTicks, PreDelayTicks, PostDelayTicks;

	ioPB.ioReqCount = 512;
	StartTicks = *Ticks;
	for (i=0; i<SeekCount/2; ) {
		ioPB.ioPosOffset = VolOffset + 1024*1024;
		if (err = PBRead(&ioPB, false))
			Error(err);
		j = 0;
		PreDelayTicks = *Ticks;
		PostDelayTicks = PreDelayTicks + (Random()&7)+1;
		while (*Ticks < PostDelayTicks)
			if (j++ == 0)
				SpinCursor(16);
		StartTicks += *Ticks - PreDelayTicks;
		ioPB.ioPosOffset = VolOffset;
		if (err = PBRead(&ioPB, false))
			Error(err);
		if (++i < SeekCount/2) {
			j = 0;
			PreDelayTicks = *Ticks;
			PostDelayTicks = PreDelayTicks + (Random()&7)+1;
			while (*Ticks < PostDelayTicks)
				if (j++ == 0)
					SpinCursor(16);
			StartTicks += *Ticks - PreDelayTicks; } }
	return *Ticks - StartTicks;
}

void Results()
	{
	char		XferRdStr[8], XferWtStr[8], AccessStr[64];
	short		itemHit;

	DisposDialog(DlogPtr);		/* take down "Please wait" */
	sprintf(XferRdStr, "%5.0f", XferRdTicks/6.0);
	sprintf(XferWtStr, "%5.0f", XferWtTicks/6.0);
	if (SkipAccess)
		strcpy(AccessStr, p2cstr(*GetResource('STR ', StrVolTooSmall)));
	else
		sprintf(AccessStr, "%5.0f", AccessTicks/6.0);
	ParamText(XferRdStr, XferWtStr, AccessStr, Version);
	Alert(ResultsAlertID, 0);
}

main()
	{
	
	if (Initialize()) {
		Calibrate();
		XferRdTicks = XferTest(TransferCount, false);	/* reads */
		XferWtTicks = XferTest(TransferCount, true);  	/* writes */
		if (!SkipAccess)
			AccessTicks = AccessTest();
		InitCursor();
		Results(); }
	ExitToShell();
}

                                                                          