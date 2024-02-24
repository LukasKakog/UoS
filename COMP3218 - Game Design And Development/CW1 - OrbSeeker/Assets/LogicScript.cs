using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class LogicScript : MonoBehaviour
{

    public PlayerScript player; 

    private bool gamePaused = false;

    public GameObject BGPlayer;

    public GameObject PauseMenu;

    public AudioSource pauseSound;

    public AudioSource unpauseSound;

    // Start is called before the first frame update
    void Start()
    {
        player = GameObject.FindGameObjectWithTag("Player").GetComponent<PlayerScript>();
    }

    // Update is called once per frame
    void Update()
    {

        if(Input.GetKeyDown(KeyCode.Escape)) {
            if (gamePaused) {

                player.unpauseGame();
                gamePaused = false;

                PauseMenu.SetActive(false);
                unpauseSound.Play();

                if(BGPlayer != null) {
                    BGPlayer.GetComponent<AudioSource>().Play();
                }

            } else {
                
                player.pauseGame();
                gamePaused = true;

                PauseMenu.SetActive(true);
                pauseSound.Play();

                if(BGPlayer != null) {
                    BGPlayer.GetComponent<AudioSource>().Stop();
                }

            }
        } else if (Input.GetKeyDown(KeyCode.R) && !gamePaused) {
            
            SceneManager.LoadScene(SceneManager.GetActiveScene().name);

        }
        
    }
}
