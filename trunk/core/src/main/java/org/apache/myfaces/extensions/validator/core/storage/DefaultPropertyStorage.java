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
package org.apache.myfaces.extensions.validator.core.storage;

import static org.apache.myfaces.extensions.validator.internal.UsageCategory.INTERNAL;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.HashMap;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(INTERNAL)
public class DefaultPropertyStorage implements PropertyStorage
{
    protected final Log logger = LogFactory.getLog(getClass());

    private Map<String, Field> fieldMap = new HashMap<String, Field>();
    private Map<String, Method> methodMap = new HashMap<String, Method>();

    public void storeField(Class targetClass, String property, Field field)
    {
        this.fieldMap.put(createKey(targetClass, property), field);
    }

    public void storeMethod(Class targetClass, String property, Method method)
    {
        this.methodMap.put(createKey(targetClass, property), method);
    }

    public Field getField(Class targetClass, String property)
    {
        return this.fieldMap.get(createKey(targetClass, property));
    }

    public Method getMethod(Class targetClass, String property)
    {
        return this.methodMap.get(createKey(targetClass, property));
    }

    public boolean containsField(Class targetClass, String property)
    {
        return this.fieldMap.containsKey(createKey(targetClass, property));
    }

    public boolean containsMethod(Class targetClass, String property)
    {
        return this.methodMap.containsKey(createKey(targetClass, property));
    }

    private String createKey(Class targetClass, String property)
    {
        return targetClass + "#" + property;
    }
}