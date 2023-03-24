class ATM{
	static int balance;
	public static void main(String Args[]){
		ATM myATM = new ATM();
		myATM.go();
	}
	public void go(){
		Toolbox myToolbox = new Toolbox();
		ATM myATM = new ATM();
		System.out.println("Welcome to online ATM banking");
		System.out.println("How much do you want in your account?");
		balance = myToolbox.readIntegerFromCmd();
		System.out.println(balance);
		System.out.println("What do you want to do?");
		System.out.println("1 : Withdraw");
		System.out.println("2 : Deposit");
		System.out.println("3 : Inquire");
		System.out.println("4 : Quit");
		int choice = myToolbox.readIntegerFromCmd();
		if (choice == 1)
			myATM.withdraw();
		else if (choice == 2)
			myATM.deposit();
		else if (choice == 3)
			myATM.inquire();
		else
			myATM.quit();
	}
	public void withdraw(){
		Toolbox myToolbox = new Toolbox();
		System.out.println("*****************************************");
		System.out.println("		Withdrawal");
		System.out.println("*****************************************");
		System.out.println("How much would you like to withdraw?");
		int w = myToolbox.readIntegerFromCmd();
		if (w>=0){
			balance = balance - w;
		}
		System.out.println("*****************************************");
		System.out.println("	Your new balance is "+balance);
		System.out.println("*****************************************");
	}
	public void deposit(){
		Toolbox myToolbox = new Toolbox();
		System.out.println("*****************************************");
		System.out.println("		Deposit");
		System.out.println("*****************************************");
		System.out.println("How much would you like to deposit?");
		int d = myToolbox.readIntegerFromCmd();
		if (d>=0){
			balance = balance + d;
		}
		System.out.println("*****************************************");
		System.out.println("	Your new balance is "+balance);
		System.out.println("*****************************************");
	}
	public void inquire(){
		System.out.println("*****************************************");
		System.out.println("	Your balance is "+balance);
		System.out.println("*****************************************");
	}
	public void quit(){
		System.out.println("*****************************************");
		System.out.println("		Goodbye!");
		System.out.println("*****************************************");
		System.exit(0);		
	}
}