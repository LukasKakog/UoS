
public class SmartCard
{
    String name;
    boolean areTheyStaff;

    public SmartCard(String x)
    {
        name = x;
        areTheyStaff = false;
    }
    
    public String getOwner(){
        return name;
    }
    
    public boolean isStaff() {
        if (areTheyStaff) {
            return true;
        } else {
            return false;
        }
    }
    
    public void setStaff (boolean staff){
        areTheyStaff = staff;
    }
   
}