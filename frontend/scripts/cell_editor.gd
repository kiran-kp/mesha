extends TextEdit

var view: Container = null
var index: int = 0
const cols: int = 3

# Called when the node enters the scene tree for the first time.
func _ready():
	self.view = $"../MainView"
	self.grab_focus()
	pass

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):
	pass

func _on_text_changed():
	var c = self.get_caret_column()
	if (c == 0):
		var t = self.text.split("\n", false)
		if (t.size() > 0):
			var result = MeshaServer.eval(t[t.size() - 1])
			var container = PanelContainer.new()
			var m = Label.new()
			container.add_child(m)
			self.view.add_child(container)
			m.set_text(result)
