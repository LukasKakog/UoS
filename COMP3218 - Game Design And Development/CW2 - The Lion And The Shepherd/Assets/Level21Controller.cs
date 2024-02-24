using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Playables;
using UnityEngine.SceneManagement;
using UnityEngine.UI;

public class Level21Controller : MonoBehaviour, LevelController {
    public GameObject UI;
    public GameObject objectiveUI;
    public GameObject pauseMenuUI;
    public GameObject winUI;

    public GameObject firstCutscene;

    public PlayerController playerController;

    private bool firstCutSceneStarted = false;
    private bool firstCutSceneFinished = false;

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
            objectiveUI.GetComponent<Text>().text = "Return home";
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

        if (firstCutSceneStarted && !firstCutSceneFinished) {
            return;
        }

        if (Input.GetKeyDown(KeyCode.Escape)) {
            if (gameState == "Playing") {
                Pause();
            } else if (gameState == "Paused") {
                Resume();
            }
        }
    }

    public void StartSecondCutScene() {
        if (!enoughBlueberries || gameState == "Win") {
            return;
        }

        Win();
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

        PlayerPrefs.SetInt("Level 3.1 Unlocked", 1);
    }
}
