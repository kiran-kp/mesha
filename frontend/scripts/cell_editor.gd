extends TextEdit

var cell = null

# Called when the node enters the scene tree for the first time.
func _ready():
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
			var result = LispServer.eval(t[t.size() - 1])
			self.insert_text_at_caret(result + "\n")
