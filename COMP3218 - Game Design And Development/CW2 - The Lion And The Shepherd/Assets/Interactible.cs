using Unity.Mathematics;
using UnityEngine;
using UnityEngine.UI;

public class Interactible : MonoBehaviour {
    public string id;
    public bool visible = false;
    public bool canInteract = true;

    public Transform alertTransform; // The position of the alert object
    public GameObject alertPrefab;

    private float fadeRate = 5f;

    private GameObject worldCanvasObject; // The parent to the image
    private GameObject alertObject;
    private Image alertImage;

    void Start() {
        gameObject.layer = LayerMask.NameToLayer("Interactible");

        worldCanvasObject = GameObject.Find("WorldCanvas");
        alertObject = Instantiate(alertPrefab);
        alertImage = alertObject.GetComponent<Image>();

        alertObject.transform.SetParent(worldCanvasObject.transform);
        alertObject.transform.position = alertTransform.position;
    }

    void Update() {
        alertObject.transform.position = alertTransform.position;
        if (visible && canInteract) {
            alertImage.color = new Color(1f, 1f, 1f, math.lerp(alertImage.color.a, 1, fadeRate * Time.deltaTime));
        } else {
            alertImage.color = new Color(1f, 1f, 1f, math.lerp(alertImage.color.a, 0, fadeRate * Time.deltaTime));
        }
    }

    // Updates a float with a given opacity between 0 and 1
    public void UpdateAlert(Quaternion rotation) {
        rotation = Quaternion.Euler(0, rotation.eulerAngles.y, 0);
        alertObject.transform.rotation = rotation;
    }

    public void Select() {
        Material mat = gameObject.GetComponent<Renderer>().material;
        mat.SetColor("_EmissionColor", Color.red);
    }
}