using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using UnityEngine;
using UnityEngine.EventSystems;

public class PushScript : MonoBehaviour
{

    //The transform point of the box object
    public Transform movePoint;
    
    //The Layer mask for immovable obstacles
    public LayerMask obstacles;

    //The Layer mask for other boxes
    public LayerMask boxes;

    //The Layer mask for doors
    public LayerMask door;

    [SerializeField] private AudioSource shiftBox;
    
    //A function that moves the box
    public void Move(Vector3 targetPos) {

        movePoint.position += targetPos;

        shiftBox.Play();
    }

    //Checks if an obstacle is present next to the box, will return true if there exists an obstacle.
    private bool ObstaclePresent(Vector3 target) {

        bool blocked = (Physics2D.OverlapCircle(target, 0.2f, obstacles) != null) || (Physics2D.OverlapCircle(target, 0.2f, boxes) != null)
        || (Physics2D.OverlapCircle(target, 0.2f, door) != null);
        

        return blocked;
    }

    //Checks if the box is blocked towards the side it's being pushed at, returns true if path is blocked.
    //Called by player avatar.
    public bool Blocked(Vector3 direction) {

        if(ObstaclePresent(direction + movePoint.position)) {
            return true;
        }

        return false;

    }

}
