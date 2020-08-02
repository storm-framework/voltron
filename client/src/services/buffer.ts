import { Buffer } from "@/types";

declare const firebase: any;
declare const ace: any;
declare const Firepad: any;

// Initialize Firebase.
// TODO: replace with your Firebase project configuration
const config = {
  apiKey: "<API_KEY>",
  authDomain: "firepad-tests.firebaseapp.com",
  databaseURL: "https://firepad-tests.firebaseio.com"
};

const initApp = firebase.initializeApp(config);
const initializedBuffers: Set<any> = new Set();

// function isInit(buf: Buffer): boolean {
//   const done = initializedBuffers[buf.id];
//   initializedBuffers[buf.id] = true;
//   console.log("isInit", buf, done);
//   return done;
// }

class BufferService {
  // codeBufferDiv(buf: Buffer) {
  //   return "editor-" + buf.id;
  // }
  // initializedBuffers: Set<number> = new Set(); 
    
  newBuffer(groupId: number): Buffer {
    const newRef = firebase.database().ref().push();
    return {
      id: groupId,
      hash: newRef.key,
      text: "",
      div: "editor-" + groupId
    }
  }

  getFirepadRef(buf: Buffer) {
    const ref = firebase.database().ref();
    return ref.child(buf.hash);
  }

  initBuf(buf: Buffer) {
    const editor = ace.edit(buf.div);
    // console.log("initBuf-0", editor);

    if (initializedBuffers.has(editor)) return;
    initializedBuffers.add(editor);

    editor.setTheme("ace/theme/textmate");
    editor.getSession().setMode("ace/mode/haskell");
    editor.setOptions({
      maxLines: 10,
      minLines: 10
    });
    const firepadRef = this.getFirepadRef(buf);
    console.log("initBuf", buf.div, editor.getValue());
    const firepad = Firepad.fromACE(firepadRef, editor, {
      defaultText:
        "-- blank"
    });

  }
}

export default new BufferService();
