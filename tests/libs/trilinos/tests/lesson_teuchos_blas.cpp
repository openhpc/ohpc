//
// https://code.google.com/p/trilinos/wiki/TeuchosBLAS
//
#include "Teuchos_ParameterList.hpp"
#include "Teuchos_StandardParameterEntryValidators.hpp"
#include "Teuchos_Array.hpp"
#include "Teuchos_Version.hpp"

int 
main (int argc, char* argv[])
{
  using Teuchos::Array;
  using Teuchos::ParameterList;
  using Teuchos::RCP;
  using Teuchos::rcp;
  using Teuchos::tuple;

  std::cout << Teuchos::Teuchos_Version() << std::endl << std::endl;

  // Create an empty parameter list.  You may also give it a name if you like.
  ParameterList My_List;

  // Set parameters in the list.  Parameters may have any type.
  // The optional third argument is a documentation string for that parameter.
  My_List.set ("Max Iters", 1550, "Maximum number of iterations in the solver");
  My_List.set ("Tolerance", 1e-10, "The tolerance used for the convergence check");

  // For the "Solver" option, create a validator that will automatically
  // check input and also create documentation for this parameter.
  // There are many kinds of validators; you may even implement 
  // your own.  This particular validator restricts the input string to
  // a finite set of values, and converts the string to an integer if it
  // is one of those values.
  typedef Teuchos::StringToIntegralParameterEntryValidator<int> validator_type;
  RCP<validator_type> solverValidator =
    rcp (new validator_type (tuple<std::string> ("GMRES", "CG", "TFQMR"), "Solver"));
 
  // Set the "Solver" parameter.  Specify the above validator; 
  // it will validate the "GMRES" input right here.
  My_List.set ("Solver", "GMRES", "The type of solver to use.", solverValidator);

  // The templated ``set'' method should cast the input value to the
  // correct data type.  However, in the case where the compiler is not casting the input
  // value to the expected data type, you may use an explicit cast.
  My_List.set ("Tolerance", static_cast<float> (1e-10), "Tolerance used for convergence check");

  // Reference-counted pointers can also be passed through a Teuchos::ParameterList.
  // To illustrate this we will use the Teuchos::Array class to create an array of 10 doubles
  // representing an initial guess for a linear solver, whose memory is being managed by a
  // Teuchos::RCP.  Be aware that serializing or printing pointer values doesn't make sense.
  RCP<Array<double> > rcp_Array = rcp (new Array<double> (10, 0.0));
  My_List.set ("Initial Guess", rcp_Array, "The initial guess as a RCP to an Array.");

  // Parameter lists may be nested.  This means that a ParameterList 
  // is a valid value in another ParameterList.  We say then that the first
  // ParameterList is a "sublist" of the second one.  We also supply a 
  // method that creates a sublist in an existing ParameterList and 
  // returns a reference to it.
  ParameterList& Prec_List = 
    My_List.sublist ("Preconditioner", false, "Sublist that defines the preconditioner.");

  // Fill the sublist with parameters.  Prec_List is a reference,
  // so the parameters will go directly into the sublist of My_List.
  Prec_List.set ("Type", "ILU", "The type of preconditioner to use");
  Prec_List.set ("Drop Tolerance", 1e-3, "Drop tolerance for incomplete factorization");

  // The parameter list can be queried about the existence
  // of a parameter, sublist, or type.  Has a solver been chosen?
  bool solver_defined = My_List.isParameter("Solver");

  // Has a preconditioner been chosen?
  bool prec_defined = My_List.isSublist("Preconditioner");

  // Has a tolerance been chosen and is it a double-precision number?
  // Note the syntax for calling a method which is templated.
  bool tol_double = My_List.isType<double> ("Tolerance");

  // Has a drop tolerance been chosen and is it a double-precision number?
  // This is a nonmember template function. 
  bool dtol_double = Teuchos::isParameterType<double>(Prec_List, "Drop Tolerance");

  // The last two methods above for checking the parameter type are equivalent.
  // Some very old compilers might not like the syntax of the first type-checking
  // method.  Thus, we offer the second type-checking method as an alternative.
 
  // Parameters can be retrieved from the parameter list in quite a few ways.
  // Here, we call the 'get' method that creates and sets the parameter if it doesn't exist.
  int its = My_List.get ("Max Iters", 1200);

  // Get method that retrieves a parameter of a particular type.
  // This will throw an exception if the parameter doesn't exist.
  // It will not modify the list in that case.
  float tol = My_List.get<float> ("Tolerance");

  /* In the above example, the first ``get'' method is a safe way of
     obtaining a parameter if you don't know that it exists yet in the list.
     The second ``get'' method should be used when you know that 
     the parameter exists in the list, since it will throw an exception 
     if the parameter doesn't exist.  The safest way to use the second 
     ``get'' method is in a try/catch block:
  */
  try {
    tol = My_List.get<float> ("Tolerance");
  }
  catch ( std::exception& e) {
    tol = 1e-6; // Set a default value.
  }

  /* The second ``get'' method uses a syntax that may not be
     acceptable to older compilers.  Optionally, there is another portable templated
     ``get'' function that can be used in the place of the second ``get'' method:
  */
  try {
    tol = Teuchos::getParameter<float> (My_List, "Tolerance");
  }
  catch ( std::exception& e) {
    tol = 1e-6;
  }

  // Get the "Solver" value and validate!
  std::string solver = 
    solverValidator->validateString (Teuchos::getParameter<std::string> (My_List, "Solver"));

  std::cout << "\n# Printing this parameter list using operator<<(...) ...\n\n";
  std::cout << My_List << std::endl;

  std::cout << "\n# Printing the parameter list only showing documentation fields ...\n\n";
  My_List.print(std::cout,Teuchos::ParameterList::PrintOptions().showDoc(true).indent(2).showTypes(true));

  return 0;
}
