#ifndef _EMPLOYEE_H
#define _EMPLOYEE_H

#include <string>

using namespace std;

class Employee {
public:
   Employee(const string&, const string&);
   virtual ~Employee() = default; 

   void setFirstName(const string&);
   string getFirstName() const;

   void setLastName(const string&);
   string getLastName() const;

   virtual double getEarning() const = 0; 
   virtual string getInfo() const;             

private:
   string firstName;
   string lastName;
};

#endif