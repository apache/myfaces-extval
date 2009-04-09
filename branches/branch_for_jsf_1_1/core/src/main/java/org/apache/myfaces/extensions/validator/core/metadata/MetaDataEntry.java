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
package org.apache.myfaces.extensions.validator.core.metadata;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.Map;
import java.util.HashMap;

/**
 * Data holder which stores the meta-data and some information where the meta-data was around.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public class MetaDataEntry
{
    protected final Log logger = LogFactory.getLog(getClass());

    private String key;
    private Object value;
    private Map<String, Object> properties = new HashMap<String, Object>();

    public String getKey()
    {
        return key;
    }

    public void setKey(String key)
    {
        if(this.logger.isTraceEnabled())
        {
            this.logger.trace("setting meta-data key: " + key);
        }

        this.key = key;
    }

    public Object getValue()
    {
        return value;
    }

    public <T> T getValue(Class<T> targetClass)
    {
        return (T)getValue();
    }

    public void setValue(Object value)
    {
        if(this.logger.isTraceEnabled())
        {
            this.logger.trace("setting meta-data value: " + value);
        }

        this.value = value;
    }

    public void setProperties(Map<String, Object> properties)
    {
        this.properties = properties;
    }

    public Object getProperty(String key)
    {
        return this.properties.get(key);
    }

    public <T> T getProperty(String key, Class<T> targetClass)
    {
        return (T)getProperty(key);
    }

    public void setProperty(String key, Object value)
    {
        if(this.logger.isTraceEnabled())
        {
            this.logger.trace("new property added key: " + key + " value: " + value + " for metadata-key: " + this.key);
        }

        this.properties.put(key, value);
    }
}
