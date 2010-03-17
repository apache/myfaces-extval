/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.test.base.util;

import javax.faces.context.FacesContext;
import java.util.Map;

/**
 * @author Gerhard Petracek
 */
//TODO
public class MethodUtils
{
    private MethodUtils()
    {
    }

    public static boolean checkMethodCalled(Class usedClass, String methodName, int callCount)
    {
        checkRequiredClass(usedClass);

        return callCount == getMethodCallCount(usedClass, methodName);
    }

    public static boolean isMethodCalled(Class usedClass, String methodName)
    {
        checkRequiredClass(usedClass);

        return FacesContext.getCurrentInstance().getExternalContext().getRequestMap()
                .containsKey(usedClass.getName() + "#" + methodName);
    }

    @SuppressWarnings({"unchecked"})
    public static void signalMethodCalled(Class usedClass, String methodName)
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

        int methodCallCount = 0;
        String key = usedClass.getName() + "#" + methodName;

        if(requestMap.containsKey(key))
        {
            methodCallCount = (Integer)requestMap.get(key);
        }

        requestMap.put(key, ++methodCallCount);
    }

    private static int getMethodCallCount(Class usedClass, String methodName)
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

        String key = usedClass.getName() + "#" + methodName;

        if(requestMap.containsKey(key))
        {
            return (Integer)requestMap.get(key);
        }

        return 0;
    }

    public static void signalClassUsed(Class mockClass)
    {
        FacesContext.getCurrentInstance().getExternalContext().getRequestMap()
                .put(mockClass.getName() + ":used", true);
    }

    public static boolean isClassUsed(Class mockClass)
    {
        return FacesContext.getCurrentInstance().getExternalContext().getRequestMap()
                .containsKey(mockClass.getName() + ":used");
    }

    private static void checkRequiredClass(Class usedClass)
    {
        if (!isClassUsed(usedClass))
        {
            throw new IllegalStateException(usedClass.getName() + " not used");
        }
    }
}