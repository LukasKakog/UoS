public class ATM{
	public static int balance;
	public static void main(String Args[]){
		ATM myATM = new ATM();
		myATM.go();
	}
	static void go(){
		ATM myATM = new ATM();
		Toolbox myToolbox = new Toolbox();
		System.out.println("Welcome to online ATM banking");
		System.out.println("How much do you want in your account?");
		balance = myToolbox.readIntegerFromCmd();
		System.out.println(balance);
		while(true){
			if (balance <= 0){
				System.out.println("*****************************************");
				System.out.println("  Invalid value. Enter a different one");
				System.out.println("*****************************************");
				System.out.println("Welcome to online ATM banking");
				System.out.println("How much do you want in your account?");
				balance = myToolbox.readIntegerFromCmd();
			} else {
				reader();
			}
		}
	}
	static void reader(){
		Toolbox myToolbox = new Toolbox();
		System.out.println("What do you want to do?");
		System.out.println("1 : Withdraw");
		System.out.println("2 : Deposit");
		System.out.println("3 : Inquire");
		System.out.println("4 : Quit");
		System.out.println("Enter your number");
		int choice = myToolbox.readIntegerFromCmd();
		while(true){
			if (choice < 1){
				System.out.println("*****************************************");
				System.out.println("  Invalid value. Enter a different one");
				System.out.println("*****************************************");
				System.out.println("What do you want to do?");
				System.out.println("1 : Withdraw");
				System.out.println("2 : Deposit");
				System.out.println("3 : Inquire");
				System.out.println("4 : Quit");
				System.out.println("Enter your number");
				choice = myToolbox.readIntegerFromCmd();
			}else if (choice > 4){
				System.out.println("*****************************************");
				System.out.println("  Invalid value. Enter a different one");
				System.out.println("*****************************************");
				System.out.println("What do you want to do?");
				System.out.println("1 : Withdraw");
				System.out.println("2 : Deposit");
				System.out.println("3 : Inquire");
				System.out.println("4 : Quit");
				System.out.println("Enter your number");
				choice = myToolbox.readIntegerFromCmd();
			}else{
				break;
			}
		}
		
		if (choice == 1)
			withdraw();
		else if (choice == 2)
			deposit();
		else if (choice == 3)
			inquire();
		else if (choice == 4)
			quit();
	}
	
	static void withdraw(){
		Toolbox myToolbox = new Toolbox();
		System.out.println("*****************************************");
		System.out.println("		Withdrawal");
		System.out.println("*****************************************");
		System.out.println("How much would you like to withdraw?");
		int w = myToolbox.readIntegerFromCmd();
		while (true){
			if (w <= 0){
				System.out.println("*****************************************");
				System.out.println("  Invalid value. Enter a different one.");
				System.out.println("*****************************************");
				System.out.println("How much would you like to withdraw?");
				w = myToolbox.readIntegerFromCmd();
			} else {
				break;
			}
		}
		while(true){
			if(balance < w){
				System.out.println("*****************************************");
				System.out.println("Withdrawal not possible due to insufficient balance.");
				System.out.println("*****************************************");
				System.out.println("How much would you like to withdraw? ");
				w = myToolbox.readIntegerFromCmd();
			} else {
				break;
			}
		}
		balance -= w;
		System.out.println("*****************************************");
		System.out.println("	Your new balance is " + balance);
		System.out.println("*****************************************");
	}
	static void deposit(){
		Toolbox myToolbox = new Toolbox();
		System.out.println("*****************************************");
		System.out.println("		Deposit");
		System.out.println("*****************************************");
		System.out.println("How much would you like to deposit?");
		System.out.println("Enter your number");
		int d = myToolbox.readIntegerFromCmd();
		while(true){
			if (d <= 0){
				System.out.println("*****************************************");
				System.out.println("  Invalid value. Enter a different one");
				System.out.println("*****************************************");
				System.out.println("How much would you like to deposit?");
				d = myToolbox.readIntegerFromCmd();
			} else {
				break;
			}
		}
		balance = balance + d;
		System.out.println("*****************************************");
		System.out.println("	Your new balance is " + balance);
		System.out.println("*****************************************");
	}
	static void inquire(){
		System.out.println("*****************************************");
		System.out.println("	Your balance is " + balance);
		System.out.println("*****************************************");
	}
	static void quit(){
		System.out.println("*****************************************");
		System.out.println("		Goodbye!");
		System.out.println("*****************************************");	
		System.exit(0);
	}
}