using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class ResetLevel : MonoBehaviour
{

    public LevelLoaderScript levelLoader;

    private void Start() {
        levelLoader = GameObject.FindGameObjectWithTag("TransitionCF").GetComponent<LevelLoaderScript>();
    }

    public void Reset() {

        levelLoader.LoadNextScene(SceneManager.GetActiveScene().buildIndex);

    }
}
