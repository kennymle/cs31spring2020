#include <iostream>
#include <string>
#include <cassert>
using namespace std;

bool hasValidSyntax(string orders);
bool isValidCode(string stateCode);
int countCases(string orders, char status, int& caseCount);

int main() {
    int caseCountfilled=0;      //+
    int caseCountunfilled = 0;  //-
    for (;;)
    {
        cout << "Enter an order string (type 'quit' to quit): " << endl;
        string orders;
        getline(cin, orders);
        if (orders == "quit")       //exit program
            break;
        if (hasValidSyntax(orders))     //if order string is valid
        {
            cout << "countCases returns " << countCases(orders, '+', caseCountfilled) << endl;
            cout << "These are the positive cases: " << caseCountfilled << endl;
            cout << "countCases returns " << countCases(orders, '-', caseCountunfilled) << endl;
            cout << "These are the negative cases: " << caseCountunfilled << endl;
        }
        else
            cout << "The order string entered was not valid." << endl;
    }
}

bool hasValidSyntax(string orders)
{
    if (orders == "")     //empty string is an order string
        return true;

    int a=0;
    while (a < orders.size())
    {
        string test = "";
        
        test += orders[a];     
        test += orders[a+1];

        if (isValidCode(test))   //checks for two consecutive letters
        {
            a += 2;                 
            if (!isdigit(orders[a]))     //if there's not a digit following the letters, return false
                return false;
            while (isdigit(orders[a]))    //increment past the numbers
                a++;
            if (orders[a] == '+' || orders[a] == '-')  //once past the numbers, increment past the + or -
                a++;
            else return false;
        }
        else
            return false;              //otherwise, return false
    }
    return true;    //returns true if all the loops are satisfied and haven't return false
}


bool isValidCode(string stateCode)
{
    const string codes =        //accounts for all combination of the abbreviation of state names
        "AL.AK.AZ.AR.CA.CO.CT.DE.DC.FL.GA.GU.HI.ID.IL.IN.IA.KS."
        "KY.LA.ME.MD.MA.MI.MN.MS.MO.MP.MT.NE.NV.NH.NJ.NM.NY.NC."
        "ND.OH.OK.OR.PA.PR.RI.SC.SD.TN.TX.UT.VT.VA.WA.WV.WI.WY"

        "al.ak.az.ar.ca.co.ct.de.dc.fl.ga.gu.hi.id.il.in.ia.ks."
        "ky.la.me.md.ma.mi.mn.ms.mo.mp.mt.ne.nv.nh.nj.nm.ny.nc."
        "nd.oh.ok.or.pa.pr.ri.sc.sd.tn.tx.ut.vt.va.wa.wv.wi.wy"

        "aL.aK.aZ.aR.cA.cO.cT.dE.dC.fL.gA.gU.hI.iD.iL.iN.iA.kS."
        "kY.lA.mE.mD.mA.mI.mN.mS.mO.mP.mT.nE.nV.nH.nJ.nM.nY.nC."
        "nD.oH.oK.oR.pA.pR.rI.sC.sD.tN.tX.uT.vT.vA.wA.wV.wI.wY"

        "Al.Ak.Az.Ar.Ca.Co.Ct.De.Dc.Fl.Ga.Gu.Hi.Id.Il.In.Ia.Ks."
        "Ky.La.Me.Md.Ma.Mi.Mn.Ms.Mo.Mp.Mt.Ne.Nv.Nh.Nj.Nm.Ny.Nc."
        "Nd.Oh.Ok.Or.Pa.Pr.Ri.Sc.Sd.Tn.Tx.Ut.Vt.Va.Wa.Wv.Wi.Wy";
    return (stateCode.size() == 2 &&
        stateCode.find('.') == string::npos &&  // no '.' in stateCode
        codes.find(stateCode) != string::npos);  // match found
}

int countCases(string orders, char status, int& caseCount)
{
    if (!hasValidSyntax(orders))
        return 1;                  //returns 1 if "orders" is not an order string
    int a = 0;
    while (a < orders.size())
    {
        if (isdigit(orders[a]) && orders[a] == '0')             //checks if there's a case of zero masks
        {
            if (!isdigit(orders[a + 1]) && !isdigit(orders[a - 1]))
                return 2;             //if at least one state order specifies zero cases of mask, return 2
            while (orders[a] == '0')
                a++;
            if (!isdigit(orders[a]))
                return 2;
        }
        a++;
    }
    if (status != '+' && status != '-')
        return 3;           //if the status isn't + or -, return 3

    caseCount = 0;         //everytime this function is called, the int "caseCount" resets to 0 in order to calculate the cases for that specific order string
    string tempcases = "";
    int b = 2;
    while (b < orders.size())
    {
        if (isdigit(orders[b]))             //scans for the digits of an order using while loop
            tempcases += orders[b];
        else if (orders[b] == '+' && status == '+')
        {
            caseCount += stoi(tempcases);         //depending on + or -, caseCount will increase
            tempcases = "";
        }
        else if (orders[b] == '-' && status == '-')
        {
            caseCount += stoi(tempcases);
            tempcases = "";
        }
        else
            tempcases = "";
        b++;
    }
    return 0;
}
    //if the program makes it through, it returns 0 after setting "caseCount" to total number of 
    //cases of masks for the state orders in "orders" that have the status indicated by "status"
