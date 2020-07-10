import { Buffer } from "@/types";

declare const firebase: any;
declare const ace: any;
declare const Firepad: any;

//Initialize Firebase.
//TODO: replace with your Firebase project configuration
const config = {
  apiKey: "<API_KEY>",
  authDomain: "firepad-tests.firebaseapp.com",
  databaseURL: "https://firepad-tests.firebaseio.com"
};

const initApp = firebase.initializeApp(config);
const initializedBuffers: { [bufId: string]: boolean } = {};

function isInit(buf: Buffer): boolean {
  const done = initializedBuffers[buf.id];
  initializedBuffers[buf.id] = true;
  console.log("isInit", buf, done);
  return done;
}

class BufferService {

  getFirepadRef(buf: Buffer) {
    const ref = firebase.database().ref();
    return ref.child(buf.hash);
  }

  makeFirepadRef():string {
    const ref = firebase.database().ref();
    const newRef = ref.push();
    console.log("makeFirepadRef", newRef.key);
    return newRef.key;
  }

  initBuf(buf: Buffer) {
    // const done: boolean = isInit(buf);
    // if (done) return;
    const editor = ace.edit(buf.div);
    // editor.setValue("");
    editor.setTheme("ace/theme/textmate");
    editor.getSession().setMode("ace/mode/haskell");
    editor.setOptions({
      maxLines: 10,
      minLines: 10
    });
    // editor.$blockScrolling = Infinity;
    const firepadRef = this.getFirepadRef(buf);
    console.log("initBuf", buf.div, editor.getValue());
    const firepad = Firepad.fromACE(firepadRef, editor, {
      defaultText:
        "-- blank"
    });
  }
}

export default new BufferService();
