import java.util.ArrayList;
public class UserGroup{
	ArrayList<User> usergroup;
	UserGroup(){
		usergroup = new ArrayList<User>();
	}
	public ArrayList<User> getUsers(){
		return usergroup;
	}
	public void addSampleData(){
		User user1 = new User("fj1","Admin","Francis1");
		usergroup.add(user1);
		User user2 = new User("fj2","User","Francis2");
		usergroup.add(user2);
		User user3 = new User("fj3","Editor","Francis3");
		usergroup.add(user3);
		User user4 = new User("fj4","User","Francis4");
		usergroup.add(user4);
		User user5 = new User("fj5","User","Francis5");
		usergroup.add(user5);
		User user6 = new User("fj6","User","Francis6");
		usergroup.add(user6);
		User user7 = new User("fj7","Editor","Francis7");
		usergroup.add(user7);
		User user8 = new User("fj8","User","Francis8");
		usergroup.add(user8);
		User user9 = new User("fj9","Admin","Francis9");
		usergroup.add(user9);
		User user10 = new User("fj10","User","Francis10");
		usergroup.add(user10);
	}
	public User getUser(int usernumber){
		return this.usergroup.get(usernumber);
	}
	public void printUsernames(){
		for(User user: usergroup){
			System.out.println(user.getUsername()+" "+user.getUserType());
		}
	}
}