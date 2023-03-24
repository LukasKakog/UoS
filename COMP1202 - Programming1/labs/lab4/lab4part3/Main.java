import java.util.Iterator;
class Main{
	public static void main(String Args[]){
	arraylistM();
	}

	private static void arraylistM(){
		UserGroup users = new UserGroup();
		users.addSampleData();
		UserGroup administrators = new UserGroup();
		Iterator<User> iterator = users.getUserIterator();
		while (iterator.hasNext()) {
			User user = iterator.next();
			if (user.getUserType().equals("admin")) {
				administrators.add(user);
			}
		}
		System.out.println("Printing all administrator names:");
		administrators.printUsernames();
		administrators.getUser(administrators.size() - 1).setUserType("user");
		System.out.println("---Printing UserGroup for all Users:---");
		users.printUsernames();
		System.out.println("---Printing administrators UserGroup:---");
		administrators.printUsernames();
	}
}