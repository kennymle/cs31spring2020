#include <iostream>
#include <cstring>  // Notice this is NOT <string>; we need it because
// isValidUppercaseStateCode uses std::strstr
#include <cctype>
#include <cassert>
using namespace std;

bool isValidUppercaseStateCode(const char stateCode[]);

bool hasValidSyntax(const char orders[])
{
    if (orders[0] == '\0')
        return true;

    int k = 0;
    while (orders[k] != '\0')
    {
        if (!isalpha(orders[k]))
            return false;
        k++;
        if (orders[k] == '\0'  || !isalpha(orders[k]))
            return false;
        k++;                    //checks first two char k=2


        //char state[] = orders[k - 2] + orders[k-1]; 
        char a = toupper(orders[k - 2]);
        char b = toupper(orders[k - 1]);

        char state[3] = { a,b, };
        if (!isValidUppercaseStateCode(state))   
            return false;

        if (orders[k] =='\0' || !isdigit(orders[k]))  //element 3, k=2
            return false;
        k++;
        while (orders[k] !='\0' && isdigit(orders[k]))
            k++;                                //k is now on non digit

        if (orders[k] == '\0' || (orders[k] != '+' && orders[k] != '-'))
            return false;
        k++;                                    //onto a letter again
    }
    return true;
}

int countCases(const char orders[], char status, int& caseCount)
{
    const int RET_OK = 0;
    const int RET_BAD_SYNTAX = 1;
    const int RET_BAD_STATE_ORDER = 2;
    const int RET_BAD_STATUS = 3;

    if (!hasValidSyntax(orders))
        return RET_BAD_SYNTAX;

    if (status != '+' && status != '-')
        return RET_BAD_STATUS;

    int result = 0;


    int k = 0;
    while (orders[k] != '\0') {
        k += 2;
        while (orders[k] == '0')
            k++;



        int stateCaseCount = 0;
        while (isdigit(orders[k]))
        {
            stateCaseCount = 10 * stateCaseCount + orders[k] - '0';
            k++;
        }
        if (stateCaseCount == 0)
            return RET_BAD_STATE_ORDER;
        if (orders[k] == status)
            result += stateCaseCount;


/*
        char stateCaseCount[1000] = "";
        int i = 0;

        while (isdigit(orders[k])) {
            stateCaseCount[i] = orders[k];
            k++; i++;
        }
        //stateCaseCount[i] = "";
        if (stateCaseCount[0] == '\0')
            return RET_BAD_STATE_ORDER;

        if (orders[k] == status) {
            result += atoi(stateCaseCount);
        }*/

        k++;
    }

    caseCount = result;

    return RET_OK;
        //must not declare any variables of type STRING
}

bool isValidUppercaseStateCode(const char stateCode[])
{
    // In a declaration of an array with initialization, you can omit
    // the number of elements and the compiler will count how many items
    // are in the initializer and use that.  For a C string, the count is
    // the number of characters in the initialization string plus one more
    // for the zero byte.
    const char codes[] =
        "AL.AK.AZ.AR.CA.CO.CT.DE.DC.FL.GA.GU.HI.ID.IL.IN.IA.KS."
        "KY.LA.ME.MD.MA.MI.MN.MS.MO.MP.MT.NE.NV.NH.NJ.NM.NY.NC."
        "ND.OH.OK.OR.PA.PR.RI.SC.SD.TN.TX.UT.VT.VA.WA.WV.WI.WY";
 
    return (isupper(stateCode[0]) && isupper(stateCode[1]) &&
        stateCode[2] == '\0' && strstr(codes, stateCode) != nullptr);

    /*  return (stateCode.size() == 2 &&
        stateCode.find('.') == string::npos &&  // no '.' in stateCode
        codes.find(stateCode) != string::npos);  // match found*/

}

int main()
{

    int cases;
    assert(hasValidSyntax("TX38-CA132+"));
    assert(!hasValidSyntax("M38-CA132+"));
    cases = -999;    // so we can detect whether countCases sets cases
    assert(countCases("TX38f-CA132+Ms6-nY290-UT006+MS8+CA15+", '+', cases) == 1 && cases == -999);
    cases = -999;    // so we can detect whether countCases leaves cases unchanged
    assert(countCases("TX38-CA132+", '%', cases) == 3 && cases == -999);
    cout << "All tests succeeded" << endl;
    
}