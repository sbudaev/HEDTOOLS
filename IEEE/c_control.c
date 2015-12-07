#include <stdio.h>
/* Time stamp: 8/1/2013 9:00 AM */
int __GFORTRAN__get_regi_in_c_sse()
	/*Get the 32 bit MXCSR register content.
	Used for managing IEEE exception flags.
	The run-time system is assumed to have a
	copy of the MXCSR register for each thread.
*/

{
	int volatile mxcsr;

#ifdef _WIN32
	_asm
	{
		stmxcsr mxcsr;
	}
#endif
#ifdef __GNUC__
	asm("stmxcsr %0": "=m"(mxcsr)); /* Save control/status of SSE here*/
#endif

	return mxcsr;
};
/*///////////////////////////////////////////////////////////////////*/
void __GFORTRAN__set_regi_in_c_sse(int *mxcsr)
{
	/*Set the 32 bit MXCSR register content.
	Used for enabling IEEE rounding and exception flags. */
	int volatile s;
	s=*mxcsr;
#ifdef _WIN32
	_asm
	{
		ldmxcsr s; /*Load value into MXCSR register.*/
	}
#endif
#ifdef __GNUC__
	asm("ldmxcsr %0": "=m"(s)); /* Load control/status of SSE here*/
#endif
}
/*///////////////////////////////////////////////////////////////////*/
int __GFORTRAN__get_cw_in_c_x87()
	/*Get the 16 bit control word register content.
	Used for managing IEEE exception flags.
	The run-time system is assumed to have a
	copy of status register for each thread.
*/
{
	int volatile s;
	short int volatile t;

#ifdef _WIN32
	_asm
	{
		fstcw t;
	}
#endif
#ifdef __GNUC__
	asm("fstcw %0": "=m"(t));
#endif
	s=t; /* Ascend to full int from short int.*/

	return s;
}
/*///////////////////////////////////////////////////////////////////*/
int __GFORTRAN__get_sw_in_c_x87()
	/*Get the 16 bit control word register content.
	Used for managing IEEE exception flags.
	The run-time system is assumed to have a
	copy of status register for each thread.
*/
{
	int volatile s;
	short int volatile t;

#ifdef _WIN32
	_asm
	{
		fstsw t;
	}
#endif
#ifdef __GNUC__
	asm("fstsw %0" : "=m"(t));
#endif
	s=t; /* Ascend to full int from short int.*/

	return s;
}
/*///////////////////////////////////////////////////////////////////*/
void __GFORTRAN__set_cw_in_c_x87(int *cw)
	/*Set the 16 bit control word register content.
	Used for managing IEEE exception flags.
	Value passed is 32 bits but only low order 16 bits loaded.*/
{
	int volatile t; /* Temp integer for part of input control word.*/
	t=*cw;
#ifdef _WIN32
	_asm
	{
		fldcw t; /*Load low order 16 bits to x87 control register.*/
	}
#endif
#ifdef __GNUC__
	asm("fldcw %0" : "=m"(t)); /*Load low order 16 bits to x87 control register.*/
#endif
}
/*///////////////////////////////////////////////////////////////////*/
void __GFORTRAN__set_sw_in_c_x87(int *sw)
	/*Set the 16 bit status word register content.
	Used for managing IEEE exception flags.
	Value passed is 32 bits but only low order 16 bits needed.
	Since there is no direct way of changing selected bits in the
	status word, the FPU state is saved, the status word changed,
	as part of the state, and then state is restored.  The new
	status word is stored before the restore.  This is an
	expensive operation.  So set as many flags to new desired
	values as possible in sw before this call.
	*/
{
	/* This is temporary space for the 108 bytes of the FPU state.*/
	int volatile s[27], t;
	t=*sw; /*Store new status word in memory.*/
#ifdef _WIN32
	_asm
	{
		fsave s; /*Store FPU state to memory.*/
	}
	s[1]=t; /*Change int of state that contains status word.*/
	_asm
	{
		frstor s; /*Restore FPU state*/
	}
#endif
#ifdef __GNUC__
	asm("fsave %0" : "=m"(s)); /* Save entire FPU state here. */
	s[1]=t; /*Change int of state that contains status word.*/
	asm("frstor %0" : "=m"(s)); /* Restore FPU state here. */
#endif
}
/*///////////////////////////////////////////////////////////////////*/
void __GFORTRAN__get_states(int *states)
{
#ifdef _WIN32
	__declspec( align( 16) )int s[128]; /* Temp space for the combined SSE and x87 states.*/
	int volatile t[27];
#endif
#ifdef __GNUC__
	int volatile s[128] __attribute__((aligned(16)));
	int volatile t[27];
#endif
	int volatile i; /* Loop index */
#pragma omp critical
	{
#ifdef _WIN32 
		_asm
		{
			fxsave s; /*Save SSE state.*/
			fsave t; /*Save FPU state.*/
		}
#endif
#ifdef __GNUC__
		asm("fxsave %0": "=m"(s)); /* Save state of SSE here*/
		asm("fsave %0" : "=m"(t)); /* Save entire FPU state here. */
#endif

		for(i=0;i<128;i++) /*Copy local store to output array*/
		{states[i]=s[i];}
		for(i=128;i<128+27;i++)
		{states[i]=t[i-128];}
	}
}
/*///////////////////////////////////////////////////////////////////*/
void __GFORTRAN__set_states(int *states)
{
#ifdef _WIN32
	__declspec( align( 16) )int s[128]; /* Temp space for the combined SSE and x87 states.*/
	int volatile t[27];
#endif
#ifdef __GNUC__
	int volatile s[128] __attribute__((aligned(16)));
	int volatile t[27];
#endif
	int volatile i; /* Loop index */
	//#pragma omp critical
	{
		for(i=0;i<128;i++) /*Copy incoming array to local store*/
		{s[i]=states[i];}
		for(i=128;i<128+27;i++)
		{t[i-128]=states[i];}
#ifdef _WIN32
		_asm
		{
			fxrstor s; /*Load SSE state.*/
			frstor t; /*Load FPU state.*/
		}
#endif
#ifdef __GNUC__
		asm("fxrstor %0": "=m"(s)); /* Restore state of SSE here*/
		asm("frstor %0" : "=m"(t)); /* Restore FPU state here. */
#endif
	}
}
/*///////////////////////////////////////////////////////////////////*/
void __GFORTRAN__frem(double *dx, double *dy, double *dz)
{
	/* Support for the gfortran version of IEEE_REM(x,y).
	This routine is called by the double precision version
	for both x and y.  In turn this main routine is called by the
	mixed type versions by ascending the data types.
	The operation is expensive primarily because the FPU
	state is saved and then restored after each use.*/

	int volatile s[27];
#ifdef _WIN32
	int ra;
	_asm
	{
		fsave s; /*Save fpu state */
		mov DWORD PTR[ra],eax; /*Save eax */
		mov eax,DWORD PTR[ebp+12];  /* Load y */
		fld QWORD PTR[eax];
		mov eax, DWORD PTR[ebp+8]; 
		fld QWORD PTR[eax]; /* Load x */
_1:   /* Branch point, for fprem1 completion*/
		fprem1; /*Compute remander of x/y */
		fnstsw ax; /* Ready to complete rem operation <=> C2 =0 */
		and ax,0800h; /* Get C2 masked off from status word.*/
		add ax,ax; /* Set flag to test C2 == 0. */
		jnz _1; /* Operation completes when C2 == 0 */

		mov eax, DWORD PTR[ebp+16]; 
		fst QWORD PTR[eax]; /* Store z */
		mov eax,DWORD PTR[ra]; /* Restore eax */
		frstor s; /*Restore fpu state */
	}
#endif

#ifdef __GNUC__
	asm("fsave %0" : "=m"(s)); /* Save entire FPU state here. */
	asm("fld %0" : "+t"(*dy));
	asm("fld %0" : "+t"(*dx));
	asm("1:":); /* Branch point, for fprem1 completion*/
	asm("fprem1":); /*Compute remander of x/y */
	asm("fnstsw %%ax":::"%ax"); /* Ready to complete rem operation <=> C2 =0 */
	asm("and $0x0800,%%ax":); /* Get C2 masked off from status word.*/
	asm("add %%ax,%%ax":); /* Set flag to test C2 == 0. */
	asm("jnz 1b":); /* Operation completes when C2 == 0 */
	/* Note backward (b) jump.*/
	asm("fstp %0": "+t"(*dz));
	asm("frstor %0" : "=m"(s)); /* Restore FPU state here. */
#endif

}

/*These two functions, fraobpc and draobpc, are typed float and double.
They allow exceptions to be triggered with various
input values.  The evaluation is d = sqrt(a/b + c), computed with the x87.
/*///////////////////////////////////////////////////////////////////*/
/*These functions are not part of the IEEE module C support.  They are used
for testing only.*/
/*///////////////////////////////////////////////////////////////////*/
float fraobpc(float *sa, float *sb, float *sc)
{
	float volatile a,b,c,d;
	a=*sa;b=*sb;c=*sc;
#ifdef _WIN32
	_asm
	{
		fld a;
		fld b;
		fdivp st(1),st;
		fld c;
		faddp st(1),st;
		fsqrt;
		fstp d;
	}
#endif
#ifdef __GNUC__
	asm("fld %0" : "+t"(*sa));
	asm("fld %0" : "+t"(*sb));
	asm("fdivrp %st(0),%st(1)");
	asm("fld %0" : "+t"(*sc));
	asm("faddp %st(0),%st(1)");
	asm("fsqrt" : );
	asm("fstp %0": "+t"(d));
#endif
	return d;
}
/*///////////////////////////////////////////////////////////////////*/
double draobpc(double *da, double *db, double *dc)
{
	double volatile a,b,c,d;
	a=*da;b=*db;c=*dc;
#ifdef _WIN32
	_asm
	{
		//mov DWORD PTR[ra],eax;
		//mov eax,DWORD PTR[ebp+8];  /* Load a */
		//fld QWORD PTR[eax];
		//mov eax,DWORD PTR[ebp+12]; /* Load b */
		//fld QWORD PTR[eax];
		//fdivp st(1),st(0);   /* a/b in st(0) */
		//mov eax,DWORD PTR[ebp+16]; /* Load c */
		//fld QWORD PTR[eax];
		//faddp st(1),st(0); /* a/b + c in st(0) */
		//fsqrt; /* sqrt(a/b+c) in st(0) */
		//mov eax,DWORD PTR[ra];
		fld a;
		fld b;
		fdivp st(1),st;
		fld c;
		faddp st(1),st;
		fsqrt;
		fstp d;
	}
#endif
#ifdef __GNUC__
	asm("fld %0" : "+t"(*da));
	asm("fld %0" : "+t"(*db));
	asm("fdivrp %st(0),%st(1)");
	asm("fld %0" : "+t"(*dc));
	asm("faddp %st(0),%st(1)");
	asm("fsqrt" : );
	asm("fstp %0": "+t"(d));
#endif
	return d;
}
/*///////////////////////////////////////////////////////////////////*/
double getpi()
{
	/*Get internal representation for \pi. For testing only.*/
#ifdef _WIN32
	_asm
	{
		fldpi;
	}
#endif
#ifdef __GNUC__
	double pi;
	asm("fldpi " :);
	asm("fstp %0": "+t"(pi));
	return pi;
#endif
}