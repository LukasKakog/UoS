using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Playables;
using UnityEngine.SceneManagement;
using UnityEngine.UI;

public class Level32Controller : MonoBehaviour, LevelController {
    public GameObject UI;
    public GameObject pauseMenuUI;
    public GameObject winUI;

    public GameObject firstCutscene;
    public GameObject secondCutscene;

    public PlayerController playerController;
    public LionController lionController;

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
        lionController.OnCutSceneStart();
        firstCutscene.SetActive(true);
        UI.SetActive(false);
        firstCutSceneStarted = true;
    }

    void Update() {
        if (firstCutscene.GetComponent<PlayableDirector>().state != PlayState.Playing && firstCutSceneStarted && !firstCutSceneFinished) {
            playerController.OnCutSceneEnd();
            lionController.OnCutSceneEnd();
            playerController.SetInteractibles(new List<string>{"Blueberry"});
            firstCutscene.SetActive(false);
            UI.SetActive(true);

            firstCutSceneFinished = true;
        }

        if (secondCutscene.GetComponent<PlayableDirector>().state != PlayState.Playing && secondCutSceneStarted && !secondCutSceneFinished) {
            playerController.OnCutSceneEnd();
            lionController.OnCutSceneEnd();
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
        if (secondCutSceneStarted) {
            return;
        }

        playerController.SetPlayerPositionAndRotation(new Vector3(0f, 0.512f, -1f), Quaternion.Euler(0f, 0f, 0f));
        lionController.transform.position = new Vector3(0f, 0.55f, 1f);
        lionController.transform.rotation = Quaternion.Euler(0f, 180f, 0f);

        playerController.OnCutSceneStart();
        lionController.OnCutSceneStart();
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
        lionController.OnCutSceneEnd();
        Cursor.lockState = CursorLockMode.Locked;
        Cursor.visible = false;
        
        Time.timeScale = 1f;
        gameState = "Playing";
    }

    private void Pause() {
        pauseMenuUI.SetActive(true);
        playerController.OnCutSceneStart();
        lionController.OnCutSceneStart();
        Cursor.lockState = CursorLockMode.None;
        Cursor.visible = true;

        Time.timeScale = 0f;
        gameState = "Paused";
    }

    private void Win() {
        winUI.SetActive(true);
        playerController.OnCutSceneStart();
        lionController.OnCutSceneStart();
        Cursor.lockState = CursorLockMode.None;
        Cursor.visible = true;

        Time.timeScale = 0f;
        gameState = "Win";
    }
}
