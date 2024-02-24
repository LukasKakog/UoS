using UnityEngine;
using UnityEngine.SceneManagement;

public class PauseGame : MonoBehaviour {
    /*
    public GameObject pauseMenuUI;

    void Start() {
        pauseMenuUI.SetActive(false);
    }

    void Update() {
        if (Input.GetKeyDown(KeyCode.Escape)) {
            if (LevelController.GameState == "Playing") {
                Pause();
            } else if (LevelController.GameState == "Paused") {
                Resume();
            }
        }
    }

    public void Quit() {
        Time.timeScale = 1f;
        SceneManager.LoadScene("Menu");
    }

    public void Restart() {
        Time.timeScale = 1f;
        SceneManager.LoadScene(SceneManager.GetActiveScene().name);
    }

    public void Resume() {
        pauseMenuUI.SetActive(false);
        
        Time.timeScale = 1f;
        LevelController.GameState = "Playing";
    }

    private void Pause() {
        pauseMenuUI.SetActive(true);

        Time.timeScale = 0f;
        LevelController.GameState = "Paused";
    }
    */
}