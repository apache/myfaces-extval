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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.INTERNAL;

import java.util.Map;
import java.util.HashMap;
import java.util.logging.Logger;

/**
 * @since x.x.3
 */
@UsageInformation(INTERNAL)
public class DefaultRendererInterceptorPropertyStorage implements RendererInterceptorPropertyStorage
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    private Map<String, Object> properties = new HashMap<String, Object>();

    public void setProperty(String key, Object value)
    {
        this.properties.put(key, value);
    }

    public Object getProperty(String key)
    {
        return this.properties.get(key);
    }

    public <T> T getProperty(String key, Class<T> targetClass)
    {
        return (T)this.properties.get(key);
    }

    public void removeProperty(String key)
    {
        this.properties.remove(key);
    }
}