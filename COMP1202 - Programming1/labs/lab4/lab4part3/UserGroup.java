import java.util.ArrayList;
import java.util.Iterator;

public class UserGroup {
	ArrayList<User> usergroup;
	UserGroup(){
		usergroup = new ArrayList<User>();
	}
	public ArrayList<User> getUsers() {
		return usergroup;
	}
	public void addSampleData() {
		User user1 = new User("fj1","admin","Francis1");
		usergroup.add(user1);
		User user2 = new User("fj2","user","Francis2");
		usergroup.add(user2);
		User user3 = new User("fj3","editor","Francis3");
		usergroup.add(user3);
		User user4 = new User("fj4","user","Francis4");
		usergroup.add(user4);
		User user5 = new User("fj5","user","Francis5");
		usergroup.add(user5);
		User user6 = new User("fj6","user","Francis6");
		usergroup.add(user6);
		User user7 = new User("fj7","editor","Francis7");
		usergroup.add(user7);
		User user8 = new User("fj8","user","Francis8");
		usergroup.add(user8);
		User user9 = new User("fj9","admin","Francis9");
		usergroup.add(user9);
		User user10 = new User("fj10","user","Francis10");
		usergroup.add(user10);
	}
	public User getUser(int usernumber) {
		return this.usergroup.get(usernumber);
	}
	public void printUsernames() {
		for(User user: usergroup){
			System.out.println(user.getUsername()+" "+user.getUserType());
		}
	}
	public void add(User user) {
		usergroup.add(user);
	}
	public int size() {
		return this.usergroup.size();
	}
	public void removeFirstUser() {
		usergroup.remove(0);
	}
	public void removeLastUser() {
		usergroup.remove(usergroup.size() - 1);
	}
	public void removeUser(String username) {
		Iterator<User> iterator = usergroup.iterator();
		
		while (iterator.hasNext()) {
			User user = iterator.next();
			
			if (user.getUsername().equals(username)) {
				iterator.remove();
				System.out.println("User " + username + " was removed successfully");
			}
		}
	}
	public Iterator<User> getUserIterator() {
		Iterator<User> iterator = usergroup.iterator();
		return iterator;
	}
	
}