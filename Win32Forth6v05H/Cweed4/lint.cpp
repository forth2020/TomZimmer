//lint -save -e123
//lint -save -e124

//lint -restore  hello
//lint -restore  -e123

/* lint -restore   */
//lint -save -e234

//    lint -e1234       ok - text 
//   lint -e1234        ok - text 
//  lint -e1234         fail      
// lint -e1234          fail      
//lint -e1234           ok        
    //    lint -e1234   ok - text
    //   lint -e1234    ok - text
    //  lint -e1234     fail
    // lint -e1234      fail 
    //lint -e1234       ok


namespace {
void MyFunction( int fred ) /* COMMENT */
{
    {
        *fred
        hello()
    }
}
}
