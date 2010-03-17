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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.INTERNAL;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(INTERNAL)
public class DefaultPropertyStorage implements PropertyStorage
{
    protected final Log logger = LogFactory.getLog(getClass());

    private Map<String, Map<String, Field>> fieldMap = new HashMap<String, Map<String, Field>>();
    private Map<String, Map<String, Method>> methodMap = new HashMap<String, Map<String, Method>>();

    public void storeField(Class targetClass, String property, Field field)
    {
        getFieldMapForClass(targetClass).put(property, field);
    }

    public void storeMethod(Class targetClass, String property, Method method)
    {
        getMethodMapForClass(targetClass).put(property,  method);
    }

    public Field getField(Class targetClass, String property)
    {
        return getFieldMapForClass(targetClass).get(property);
    }

    public Method getMethod(Class targetClass, String property)
    {
        return getMethodMapForClass(targetClass).get(property);
    }

    public boolean containsField(Class targetClass, String property)
    {
        return getFieldMapForClass(targetClass).containsKey(property);
    }

    public boolean containsMethod(Class targetClass, String property)
    {
        return getMethodMapForClass(targetClass).containsKey(property);
    }

    private Map<String, Field> getFieldMapForClass(Class target)
    {
        String key = ProxyUtils.getClassName(target);
        if (!this.fieldMap.containsKey(key))
        {
            this.fieldMap.put(key, new HashMap<String, Field>());
        }
        return this.fieldMap.get(key);
    }

    private Map<String, Method> getMethodMapForClass(Class target)
    {
        String key = ProxyUtils.getClassName(target);
        if (!this.methodMap.containsKey(key))
        {
            this.methodMap.put(key, new HashMap<String, Method>());
        }
        return this.methodMap.get(key);
    }
}
