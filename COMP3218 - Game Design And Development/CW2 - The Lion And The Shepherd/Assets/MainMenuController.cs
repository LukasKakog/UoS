using UnityEngine;
using UnityEngine.SceneManagement;

public class MainMenuController : MonoBehaviour {
    void Start() {
        Cursor.lockState = CursorLockMode.None;
        Cursor.visible = true;
    }

    public void OnPlayButtonPress() {
        SceneManager.LoadScene("Levels");
    }

    public void OnQuitButtonPress() {
        Debug.Log("Quitting...");
        Application.Quit();
    }
}