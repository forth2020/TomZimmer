{
    1. ************* OK
    1. ************* OK
    1. Not OK        1
    switch
    {
    case 7: ************ OK
    case 7: Not OK 2
    case 7: Not OK   3
    case(x): *********** OK
    case     *********** OK
        9:  *********** OK
    default: *********** OK
    default :*********** OK
    default  *********** OK
        Not OK          4
        :;   *********** OK
        {    *********** OK
            function(); ************ OK
        }    *********** OK
        /* *********** OK */
         /* Not OK  5 */
       /* Not OK    6 */
        // *************** OK
       // Not Ok    7
         // *************** OK
    }
    5. But not this line ********* OK
}
}
7. 01234567890123456789012345678901234567890123456789012345678901234567890123456789ABCDEF
8. some     t       a      b          s