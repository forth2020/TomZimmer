anew cpu.f  \ November 21st, 2001 - 12:29 for Win32Forth
needs toolset.f

\ Based on the P4 Instruction Set Reference of Intel.
\ Note: Brand strings are not included.


decimal 

code cpuid ( initial_EAX_value   - ebx edx ecx eax )
  mov   eax, ebx        \ put initial_EAX_value in eax
  push  edx             \ save edx ( up ) cpuid will use edx for feature
  cpuid
  sub   ebp, # 4
  mov   0    [ebp], edx \ move edx to the returnstack
  pop   edx             \ restore edx ( up )
  push  ebx
  mov   ebx, 0 [ebp]    \ move the old edx from the returnstack to ebx
  add   ebp, # 4
  push  ebx             \ push ebx on the stack
  push  ecx             \ push ecx in nos
  mov   ebx, eax        \ put eax in tos
next c;

: verdor$ ( ebx esx ecx - adr count )
     pad 8 + !
     pad 4 + !
     pad !
     pad 12
 ;

: max_cpu_input_value ( - max_cpu_input_value ) 0 cpuid >r 3drop r>    ;
: id-vendor-cpu       ( - adr count )           0 cpuid drop verdor$   ;
: cpu_type            ( - cpu_type )            1 cpuid >r 3drop r>    ;
: brand_index         ( - brand-index )         1 cpuid 3drop low-byte ;
: cpu_feature         ( - feature )             1 cpuid 2drop nip      ;
: test_feature        ( bit# - true/false )  cpu_feature swap test-bit ;


: fpu?          ( - true/false )     0 test_feature ;
: vme?          ( - true/false )     1 test_feature ;
: de?           ( - true/false )     2 test_feature ;
: pse?          ( - true/false )     3 test_feature ;
: tsc?          ( - true/false )     4 test_feature ;
: msr?          ( - true/false )     5 test_feature ;
: pae?          ( - true/false )     6 test_feature ;
: mce?          ( - true/false )     7 test_feature ;
: cx8?          ( - true/false )     8 test_feature ;
: apic?         ( - true/false )     9 test_feature ;

: sep?          ( - true/false )    11 test_feature ;
: mtrr?         ( - true/false )    12 test_feature ;
: pge?          ( - true/false )    13 test_feature ;
: mca?          ( - true/false )    14 test_feature ;
: cmov?         ( - true/false )    15 test_feature ;
: pat?          ( - true/false )    16 test_feature ;
: pse-36?       ( - true/false )    17 test_feature ;
: psn?          ( - true/false )    18 test_feature ;
: clfsh?        ( - true/false )    19 test_feature ;
: ds?           ( - true/false )    21 test_feature ;
: acpi?         ( - true/false )    22 test_feature ;
: mmx?          ( - true/false )    23 test_feature ;
: fxsr?         ( - true/false )    24 test_feature ;
: sse?          ( - true/false )    25 test_feature ;
: sse2?         ( - true/false )    26 test_feature ;
: ss?           ( - true/false )    27 test_feature ;

: tm?           ( - true/false )    29 test_feature ;

: .features_cpu ( - )
        fpu?    if cr ." FPU    x87 FPU on Chip " then
        vme?    if cr ." VME    Virtual-8086 Mode Enhancement" then
        de?     if cr ." DE     Debugging Extensions" then
        pse?    if cr ." PSE    Page Size Extensions" then
        tsc?    if cr ." TSC    Time Stamp Counter" then
        msr?    if cr ." MSR    RDMSR and WRMSR Support" then
        pae?    if cr ." PAE    Physical Address Extensions" then
        mce?    if cr ." MCE    Machine Check Exception" then
        cx8?    if cr ." CX8    CMPXCHG8B Inst." then
        apic?   if cr ." APIC   APIC on Chip" then
        sep?    if cr ." SEP    SYSENTER and SYSEXIT" then
        mtrr?   if cr ." MTRR   Memory Type Range Registers" then
        pge?    if cr ." PGE    PTE Global Bit" then
        mca?    if cr ." MCA    Machine Check Architecture" then
        cmov?   if cr ." CMOV   Conditional Move/Compare Instruction" then
        pat?    if cr ." PAT    Page Attribute Table" then
        pse-36? if cr ." PSE-36 32-Bit Page Size Extension" then
        psn?    if cr ." PSN    Processor Serial Number" then
        clfsh?  if cr ." CLFSH  CFLUSH instruction" then
        ds?     if cr ." DS     Debug Store" then
        acpi?   if cr ." ACPI   Thermal Monitor and Clock Ctrl" then
        mmx?    if cr ." MMX    MMX Technology" then
        fxsr?   if cr ." FXSR   FXSAVE/FXRSTOR" then
        sse?    if cr ." SSE    SSE Extensions" then
        sse2?   if cr ." SSE2   SSE2 Extensions" then
        ss?     if cr ." SS     Self Snoop" then
        tm?     if cr ." TM     Therm. Monitor" then
 ;

: decode-cpu-version  ( - Reserved Type Family Model Stepping  )
   cpu_type dup>r     \ Short: R T F M S
      0xFFFFC000 and 14 rshift
   r@ 0x3000 and     12 rshift
   r@ 0xf00 and       8 rshift
   r@ 0xf0 and        4 rshift
   r> 0xf and
  ;

\ I found the following values:

\ R T F M S CPU
\ 0 0 5 1 7 P66
\ 0 0 5 7 0 P100
\ 0 0 6 5 1 PII 400

hex

\ Note: The following definition might be changed, because the lines
\ might be too long for the Internet.
\ Be sure each string between OF and ENDOF is on ONE line.

: .cache/tlb_descr ( cache/tlb_descr. - )
    case
00 of ." Null descriptor. " endof
01 of ." Instruction TLB: 4K-Byte Pages, 4-way set associative, 32 entries." endof
02 of ." Instruction TLB: 4M-Byte Pages, 4-way set associative, 2 entries." endof
03 of ." Data TLB: 4K-Byte Pages, 4-way set associative, 64 entries." endof
04 of ." Data TLB: 4M-Byte Pages, 4-way set associative, 8 entries." endof
06 of ." 1st-level instruction cache: 8K Bytes, 4-way set associative, 32 byte line size." endof
08 of ." 1st-level instruction cache: 16K Bytes, 4-way set associative, 32 byte line size." endof
0A of ." 1st-level data cache: 8K Bytes, 2-way set associative, 32 byte line size." endof
0C of ." 1st-level data cache: 16K Bytes, 4-way set associative, 32 byte line size." endof
22 of ." 3rd-level cache: 512K Bytes, 4-way set associative, 64 byte line size." endof
23 of ." 3rd-level cache: 1M Bytes, 8-way set associative, 64 byte line size." endof
25 of ." 3rd-level cache: 2M Bytes, 8-way set associative, 64 byte line size." endof
29 of ." 3rd-level cache: 4M Bytes, 8-way set associative, 64 byte line size." endof
40 of ." No 2nd-level cache or, if processor contains a valid 2nd-level cache, no 3rd-level cache." endof
41 of ." 2nd-level cache: 128K Bytes, 4-way set associative, 32 byte line size." endof
42 of ." 2nd-level cache: 256K Bytes, 4-way set associative, 32 byte line size." endof
43 of ." 2nd-level cache: 512K Bytes, 4-way set associative, 32 byte line size." endof
44 of ." 2nd-level cache: 1M Byte, 4-way set associative, 32 byte line size." endof
45 of ." 2nd-level cache: 2M Byte, 4-way set associative, 32 byte line size." endof
50 of ." Instruction TLB: 4-KByte and 2-MByte or 4-MByte pages, 64 entries." endof
51 of ." Instruction TLB: 4-KByte and 2-MByte or 4-MByte pages, 128 entries." endof
52 of ." Instruction TLB: 4-KByte and 2-MByte or 4-MByte pages, 256 entries." endof
5B of ." Data TLB: 4-KByte and 4-MByte pages, 64 entries." endof
5C of ." Data TLB: 4-KByte and 4-MByte pages,128 entries." endof
5D of ." Data TLB: 4-KByte and 4-MByte pages,256 entries." endof
66 of ." 1st-level data cache: 8KB, 4-way set associative, 64 byte line size." endof
67 of ." 1st-level data cache: 16KB, 4-way set associative, 64 byte line size." endof
68 of ." 1st-level data cache: 32KB, 4-way set associative, 64 byte line size." endof
70 of ." Trace cache: 12K-µop, 8-way set associative." endof
71 of ." Trace cache: 16K-µop, 8-way set associative." endof
72 of ." Trace cache: 32K-µop, 8-way set associative." endof
79 of ." 2nd-level cache: 128KB, 8-way set associative, sectored, 64 byte line size." endof
7A of ." 2nd-level cache: 256KB, 8-way set associative, sectored, 64 byte line size." endof
7B of ." 2nd-level cache: 512KB, 8-way set associative, sectored, 64 byte line size." endof
7C of ." 2nd-level cache: 1MB, 8-way set associative, sectored, 64 byte line size." endof
82 of ." 2nd-level cache: 256K Bytes, 8-way set associative, 32 byte line size." endof
84 of ." 2nd-level cache: 1M Byte, 8-way set associative, 32 byte line size." endof
85 of ." 2nd-level cache: 2M Byte, 8-way set associative, 32 byte line size." endof
               ." Cache type is unknown."
    endcase
  ;

decimal

: l2 ( cache/tlb_descr. - l2-cache-size )
    case
        65 of  128  endof
        66 of  256  endof
        67 of  512  endof
        68 of 1000  endof
        69 of 2000  endof

       121 of  128  endof
       122 of  256  endof
       123 of  512  endof
       124 of 1000  endof

       130 of  256  endof
       132 of 1000  endof
       133 of 2000  endof
               0 swap
    endcase
  ;

0 value l2-size

: decode-cache/tlb ( cache/tlb_descr. - )
   0 to l2-size
   4 0 do dup low-byte dup 0>
           if   cr dup l2 dup 0>
                if   to l2-size
                else  drop
                then
           .cache/tlb_descr
           else drop
           then
       8 rshift
       loop drop
 ;

: get_cache/tlb_descr ( - cache/tlb_descr K )
  2 cpuid dup>r 0xFFFFFFF0 and r> low-byte dup
  1 > abort" Cache/tlb description out of range. " ;

: .cache/tlb ( - )
  get_cache/tlb_descr 1 max 4* 0
    do   dup 0> over 31 test-bit 0= and \ cache/tlb_descr. valid
             if    decode-cache/tlb
             else  drop
             then
    loop
 ;

((

: show-l2
    s" L2.exe V2.0 at http://home.planet.nl/~josv"
    .cache/tlb
    s" The size of the L2 cache is " buffer place
    l2-size  dup 0>
       if    0 (d.) buffer +place  s" K."
       else  s" not detected."
       then
    buffer +place
    buffer count infobox bye
  ;

 ' show-l2 turnkey l2 \ ))


: .cpu
  cls .id-user
  cr .month,day,year space .time ."  CPU information."
  cr cr ." Version:"
  cr ." Vendor id cpu: " id-vendor-cpu type
  cr decode-cpu-version
     ." Brand-index:" brand_index .
     ." Type:"  3 roll . rot ." Family:" . ." Model:" swap .
     ." Stepping:" . drop
  cr cr ." Cache description:" .cache/tlb
  cr cr ." Features:" .features_cpu
 ;

code tsc  ( - tsc_low tsc_high )  \ tsc = Time Stamp Counter Pentium
  push ebx      \ save tos
  push edx
  rdtsc         \ uses edx and eax
  mov ebx, edx  \ edx to tos
  pop edx       \ restore edx
  push eax      \ eax to nos
 next c;

: clockcount ( ms - clockcount_low clockcount_high )
   get-priority >r tsc? not abort" No TSC present."
   REALTIME_PRIORITY_CLASS set-priority 30 ms
   tsc rot call Sleep tsc
   r> set-priority
   rot drop  d- dabs
 ;

0 value sum_clockcount

: clock-test
  cr ." Clock test: "
  1 0
    do  0 to  sum_clockcount
        3 dup>r 0
            do  100 clockcount d>f 100000e f/ f>s +to sum_clockcount
            loop
        1+ sum_clockcount space r> / 1 u,.r s" ,000,000" type
    loop
 ;

\s

.cpu cr clock-test

\s

