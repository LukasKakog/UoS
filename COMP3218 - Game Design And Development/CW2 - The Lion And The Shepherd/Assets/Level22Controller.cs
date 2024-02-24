using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Playables;
using UnityEngine.SceneManagement;
using UnityEngine.UI;

public class Level22Controller : MonoBehaviour, LevelController {
    public GameObject UI;
    public GameObject objectiveUI;
    public GameObject pauseMenuUI;
    public GameObject winUI;

    public GameObject firstCutscene;
    public GameObject secondCutscene;

    public PlayerController playerController;
    public Transform lion;

    private bool firstCutSceneStarted = false;
    private bool firstCutSceneFinished = false;
    private bool secondCutSceneStarted = false;
    private bool secondCutSceneFinished = false;

    public string gameState = "Playing";

    public int blueberriesRequired = 25;
    public bool enoughBlueberries = false;

    void Start() {
        pauseMenuUI.SetActive(false);
        StartCoroutine(LateStart());
    }

    IEnumerator LateStart() {
        yield return new WaitForSeconds(0.1f);
        playerController.OnCutSceneStart();
        firstCutscene.SetActive(true);
        UI.SetActive(false);
        firstCutSceneStarted = true;
    }

    void Update() {
        enoughBlueberries = blueberriesRequired <= playerController.GetBlueberriesCollected();
        if (enoughBlueberries) {
            playerController.SetInteractibles(new List<string>());
            objectiveUI.GetComponent<Text>().text = "Return to the lioness";
        } else {
            objectiveUI.GetComponent<Text>().text = String.Format("Collect {0} more blueberries", blueberriesRequired - playerController.GetBlueberriesCollected());
        }

        if (firstCutscene.GetComponent<PlayableDirector>().state != PlayState.Playing && firstCutSceneStarted && !firstCutSceneFinished) {
            playerController.OnCutSceneEnd();
            playerController.SetInteractibles(new List<string>{"Blueberry"});
            firstCutscene.SetActive(false);
            UI.SetActive(true);

            firstCutSceneFinished = true;
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
    }

    public void StartSecondCutScene() {
        if (secondCutSceneStarted || !enoughBlueberries) {
            return;
        }

        playerController.SetPlayerPositionAndRotation(new Vector3(34.5f, 3.45f, 16.5f), Quaternion.Euler(0f, 140f, 0f));
        lion.position = new Vector3(36f, 3.5f, 15f);
        lion.rotation = Quaternion.Euler(0f, -40f, 0f);

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

        PlayerPrefs.SetInt("Level 3.2 Unlocked", 1);
    }
}
