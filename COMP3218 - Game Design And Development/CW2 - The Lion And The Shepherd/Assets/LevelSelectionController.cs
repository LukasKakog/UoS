using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UI;

public class LevelSelectionController : MonoBehaviour {
    public GameObject levelButtonUIParent;

    void Start() {
        Cursor.lockState = CursorLockMode.None;
        Cursor.visible = true;

        List<string> allLevels = new List<string> {
            "1",
            "2.1",
            "3.1",
            "2.2",
            "3.2"};

        PlayerPrefs.SetInt("Level 1 Unlocked", 1);

        foreach (string level in allLevels) {
            GameObject levelButtonUI = levelButtonUIParent.transform.Find(String.Format("Level {0}", level)).gameObject;
            if (PlayerPrefs.GetInt(String.Format("Level {0} Unlocked", level), 0) == 0) {
                levelButtonUI.transform.Find("Level").gameObject.SetActive(false);
                levelButtonUI.transform.Find("Lock").gameObject.SetActive(true);

                levelButtonUI.GetComponent<Button>().enabled = false;
            } else {
                levelButtonUI.transform.Find("Level").gameObject.SetActive(true);
                levelButtonUI.transform.Find("Lock").gameObject.SetActive(false);

                levelButtonUI.GetComponent<Button>().enabled = true;
            }
        }
    }

    public void LevelButtonPress(string level) {
        if (PlayerPrefs.GetInt(String.Format("Level {0} Unlocked", level), 0) == 1) {
            SceneManager.LoadScene(String.Format("Level {0}", level));
        }
    }

    public void Level1ButtonPress() {
        SceneManager.LoadScene("Level 1");
    }

    public void Level2ButtonPress() {
        if (PlayerPrefs.GetInt("Level 2 Unlocked", 0) == 1) {
            SceneManager.LoadScene("Level 2");
        }
    }

    public void BackButtonPress() {
        SceneManager.LoadScene("Menu");
    }
}