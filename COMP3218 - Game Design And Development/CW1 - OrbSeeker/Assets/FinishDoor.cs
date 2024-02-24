using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class FinishDoor : MonoBehaviour
{
    public LevelLoaderScript levelLoader;

    private void Start() {
        levelLoader = GameObject.FindGameObjectWithTag("TransitionCF").GetComponent<LevelLoaderScript>();
    }

    private void OnTriggerEnter2D(Collider2D collision) {
        
        if (collision.gameObject.name == "Player"){
            Invoke("CompleteLevel", 0.5f);
        }
    }

    public void CompleteLevel(){
        levelLoader.LoadNextScene(SceneManager.GetActiveScene().buildIndex + 1);
    }
}   
