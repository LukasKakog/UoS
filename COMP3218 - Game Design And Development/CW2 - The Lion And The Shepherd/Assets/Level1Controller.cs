using System.Collections;
using UnityEngine;
using UnityEngine.Playables;
using UnityEngine.SceneManagement;

public class Level1Controller : MonoBehaviour, LevelController {
    public GameObject UI;
    public GameObject pauseMenuUI;
    public GameObject winUI;
    public GameObject loseUI;
    public GameObject tooltipUI;

    public GameObject firstCutscene;
    public GameObject secondCutscene;

    public PlayerController playerController;
    public SheepController sheepController;

    private bool firstCutSceneStarted = false;
    private bool firstCutSceneFinished = false;
    private bool secondCutSceneStarted = false;
    private bool secondCutSceneFinished = false;

    public string gameState = "Playing";

    void Start() {
        pauseMenuUI.SetActive(false);
        StartCoroutine(LateStart());
    }

    IEnumerator LateStart() {
        yield return new WaitForSeconds(0.1f);
        playerController.OnCutSceneStart();
        sheepController.OnCutSceneStart();
        firstCutscene.SetActive(true);
        UI.SetActive(false);
        firstCutSceneStarted = true;
    }

    IEnumerator HideTooltips() {
        yield return new WaitForSeconds(8f);
        tooltipUI.SetActive(false);
    }

    void Update() {
        if (firstCutscene.GetComponent<PlayableDirector>().state != PlayState.Playing && firstCutSceneStarted && !firstCutSceneFinished) {
            playerController.OnCutSceneEnd();
            sheepController.OnCutSceneEnd();
            firstCutscene.SetActive(false);
            UI.SetActive(true);
            firstCutSceneFinished = true;

            StartCoroutine(HideTooltips());
        }

        if (secondCutscene.GetComponent<PlayableDirector>().state != PlayState.Playing && secondCutSceneStarted && !secondCutSceneFinished) {
            playerController.OnCutSceneEnd();
            secondCutscene.SetActive(false);
            UI.SetActive(true);
            secondCutSceneFinished = true;
        }

        if ((firstCutSceneStarted && !firstCutSceneFinished) || (secondCutSceneStarted && !secondCutSceneFinished)) {
            return;
        }

        if (Input.GetKeyDown(KeyCode.Escape)) {
            if (gameState == "Playing") {
                Pause();
            } else if (gameState == "Paused") {
                Resume();
            }
        }

        if (gameState == "Playing" && secondCutSceneFinished) {
            Win();
        }

        if (gameState == "Playing" && Vector3.Distance(playerController.transform.position, sheepController.transform.position) > 25) {
            Lose();
        }
    }

    public void StartSecondCutScene() {
        if (secondCutSceneStarted) {
            return;
        }

        playerController.SetPlayerPositionAndRotation(new Vector3(35f, 3.2f, 20f), Quaternion.Euler(0f, 150f, 0f));
        playerController.OnCutSceneStart();
        secondCutscene.SetActive(true);
        UI.SetActive(false);
        secondCutSceneStarted = true;
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
        playerController.OnCutSceneEnd();
        Cursor.lockState = CursorLockMode.Locked;
        Cursor.visible = false;
        
        Time.timeScale = 1f;
        gameState = "Playing";
    }

    private void Pause() {
        pauseMenuUI.SetActive(true);
        playerController.OnCutSceneStart();
        Cursor.lockState = CursorLockMode.None;
        Cursor.visible = true;

        Time.timeScale = 0f;
        gameState = "Paused";
    }

    private void Win() {
        winUI.SetActive(true);
        playerController.OnCutSceneStart();
        Cursor.lockState = CursorLockMode.None;
        Cursor.visible = true;

        Time.timeScale = 0f;
        gameState = "Win";

        PlayerPrefs.SetInt("Level 2.1 Unlocked", 1);
        PlayerPrefs.SetInt("Level 2.2 Unlocked", 1);
    }

    private void Lose() {
        loseUI.SetActive(true);
        playerController.OnCutSceneStart();
        Cursor.lockState = CursorLockMode.None;
        Cursor.visible = true;

        Time.timeScale = 0f;
        gameState = "Lose";
    }
}
