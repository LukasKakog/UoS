using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class StartScript : MonoBehaviour
{

    public LevelLoaderScript levelLoader;

    private void Start() {
        levelLoader = GameObject.FindGameObjectWithTag("TransitionCF").GetComponent<LevelLoaderScript>();
    }

    public void StartGame()
    {
        levelLoader.LoadNextScene(4);
    }

}
