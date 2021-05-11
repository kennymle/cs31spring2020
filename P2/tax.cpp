#include <iostream>
#include <string>
using namespace std;

string name;		//defining the datas with int's, string's and double's (for monetary values) accordingly
double income;
string occ;
int children;
double taxed;

int main()
{
	cout.setf(ios::fixed);
	cout.precision(2);
	cout << "What is your name? ";				//cout's ask questions so the corresponding cin's feed in the essential data for the program to use
	getline(cin, name);
	cout << "What is your taxable income? ";
	cin >> income;
	cin.ignore(10000, '\n');			//since we read a number and the next thing we will be reading is a string
	cout << "What is your occupation? ";
	getline(cin, occ);
	cout << "How many children do you have? ";
	cin >> children;
	cout << "---" << endl;

	if (income <= 50000)			//if chain to define which operation to perform depending on the input of "income"
		taxed = (income * .04) - (200 * children);
	else if (income <= 120000 && (occ == "scientist" || occ == "engineer"))
		taxed = ((income - 50000) * .05) + 2000 - (200 * children);				//2000 represents the first 50000*4%
	else if (income <= 120000)
		taxed = ((income - 50000) * .06) + 2000 - (200 * children);
	else if (occ == "scientist" || occ == "engineer")
		taxed = ((income - 120000) * .09) + 2000 + 3500;
	else taxed = ((income - 120000) * .09) + 2000 + 4200;


	cout.setf(ios::fixed);	// giving two decimal values to the "taxed" result
	cout.precision(2);
	if (name == "")				// this if chain analyzes all the inputs to see if it meets the criteria
		cout << "You must enter a name";
	else if (income < 0)
		cout << "The taxable income must be nonnegative";
	else if (occ == "")
		cout << "You must enter an occupation";
	else if (children < 0)
		cout << "The number of children must be nonnegative";
	else if (taxed < 0)
		cout << name << " would pay $0" << endl;
	else cout << name << " would pay $" << taxed << endl;
}