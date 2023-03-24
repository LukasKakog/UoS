public class User{
	private String username,userType,name;
	User(String username, String userType, String name){
		this.username= username;
		this.userType= userType;
		this.name= name;
	}
	public String getUsername(){
		return this.username;
	}
	public String getUserType(){
		return this.userType;
	}
	public String getName(){
		return this.name;
	}
	public void setUserType(String type){
		this.userType = type;
	}
}