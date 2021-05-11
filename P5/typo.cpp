#include <iostream>
#include <string>
#include <cassert>
using namespace std;

int scoreTypo(const string dictionary[], int n, string word)
{
    if (n < 1)
        return -1;
    for (int i = 0; i < n; i++) {     //iterate through element
        if (dictionary[i] == word)                            //match
            return 0;

        if (dictionary[i].size() == word.length() - 1) {      //1 extra char
            for (int t = 0; t < word.length(); t++) {         
                string temp = word;
                temp.erase(t,1);                   //.erase(beginning index, #char after)
                if (dictionary[i] == temp)
                    return 1;
            }
        }

        if (dictionary[i].size() == word.length() + 1) {  //1 less char
            int count = 0;              
            int k = 0;
            for (int t = 0; t < dictionary[i].length(); t++) {  //must be dictionary length (larger string)
                if (dictionary[i][t] == word[k]) {
                    count++;
                    k++;
                }
            }
            if (count == dictionary[i].size() - 1)  
                return 1;
        }

        if (dictionary[i].size() == word.length()) {     //replace 1 char
            int count = 0;
            for (int t = 0; t < word.length(); t++) {
                if (dictionary[i][t] == word[t]) {
                    count++;
                }
            }
            if (count == dictionary[i].size() - 1)  // 1 less match
                return 1;
        }

        if (dictionary[i].size() == word.length()) {     //swap adj char
            for (int t = 0; t < word.length()-1; t++) {
                string temp = word;
                swap(temp[t], temp[t+1]);
                if (dictionary[i] == temp)
                    return 1;
            }
        }
           
    }
    return 2;
}

int main()
{
    // Here are some tests.  You may add more if you wish.
    string dict1[6] = { "february", "pointer", "country", "forty", "conversation", "minuscule" };



    assert(scoreTypo(dict1, 0, "forty") == -1);
    assert(scoreTypo(dict1, 6, "forty") == 0);
    assert(scoreTypo(dict1, 6, "fourty") == 1);
    assert(scoreTypo(dict1, 6, "febuary") == 1);
    assert(scoreTypo(dict1, 6, "miniscule") == 1);
    assert(scoreTypo(dict1, 6, "poitner") == 1);
    assert(scoreTypo(dict1, 6, "conservation") == 2);
    cout << "All tests succeeded" << endl;
}