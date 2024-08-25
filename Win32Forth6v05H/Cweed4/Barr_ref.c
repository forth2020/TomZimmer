// Barr.c  testing the Barr Coding Standard (2018) Rules 3.1 Spaces.

void MyFunction( uint8_t myValue )
{
    uint8_t i;
    uint8_t myArray [ 32 ]  ;
    if( !(myArray [ 17 ]==0) )  
    {
        myOtherFunction ( myArray );
    }
    int a   =10   , b=  20, c ;
    c = (a < b) ? a : b;
    c=(a<b)?a:b;
    printf("%d==123", c);

    switch( c )
    {
        case 0 : break;
        case 1: break;
        case 2  : break;
    }

    if( myArray [ 0 ] != 0 )
    {
        myOtherFunction ( myArray );
    }
    for( i=0;i<16; i++ )
    {
        myOtherFunction ( myArray );
    }
    while(  123 ) 
    {
        myOtherFunction ( myArray );
    }
    switch(  myArray [ 1 ]  )
    {
        myOtherFunction   (   myArray   );
    }
}

void MyFunction_noSpace( uint8_t myValue )
{
    uint8_t i;
    uint8_t myArray [ 32 ];

    if( myArray [ 0 ] != 0 )
    {
        myOtherFunction ( myArray );
    }
}

extern void MyFunction( uint8_t myValue );
static void MyFunction_noSpace( uint8_t myValue );

