import { Instructor, EnrollInfo, Enroll, Buffer } from "@/types";
import BufferService from "@/services/buffer";

function getNewGroups(oldBuffers: Buffer[], infos: EnrollInfo[]): string[] {
  return ["TODO"]; // FIXME
}
  
function makeBuffer(id: string): Buffer {
  const hash = BufferService.makeFirepadRef();
  const text = "-- Empty Buffer Default Text";
  const div = "";
  return {id, hash, text, div};
}

class EnrollService {
 
  makeEnroll(classData: Instructor, enrolls: EnrollInfo[] | null): Enroll | null {

    if (!enrolls) return null;

    const className = classData.class;
    const newGroups = getNewGroups(classData.allBuffers, enrolls);
    const buffers = newGroups.map(makeBuffer); 
    return { className, buffers, enrolls };

  }

}

export default new EnrollService();
