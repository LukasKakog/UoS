
public class CardLock
{
    SmartCard lastcardseen;
    Boolean lock = false;
    
    public void swipeCard(SmartCard y)
    {
        this.lastcardseen = y;
    }

    public SmartCard getLastCardSeen()
    {
        return lastcardseen;
    }
    
    public boolean isUnlocked(){
        if(getLastCardSeen().isStaff()){
            lock = getLastCardSeen().isStaff();
        }
        return lock;
    }
    
    public void toggleStudentAccess() {
        lock = !lock;
    }
    
}
