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

import java.util.Map;
import java.util.HashMap;
import java.util.logging.Logger;

/**
 * Data holder which stores the meta-data and some information where the meta-data was found.
 *
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public class MetaDataEntry
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    private String key;
    private Object value;
    private Map<String, Object> properties = new HashMap<String, Object>();

    /**
     * Returns the key which identifies the meta-data.
     *
     * @return key of the meta-data.
     */
    public String getKey()
    {
        return key;
    }

    /**
     * Sets the key of the meta-data.
     * It's suggested to use the fully qualified name of a constraint.
     *
     * @param key value identifies the the meta-data stored in the instance.
     */
    public void setKey(String key)
    {
        this.logger.finest("setting meta-data key: " + key);

        this.key = key;
    }

    /**
     * Returns the meta-data or a data-structure which represents the meta-data.
     *
     * @return the meta-data or a data-structure which represents the meta-data.
     */
    public Object getValue()
    {
        return value;
    }

    /**
     * Returns the meta-data or a data-structure which represents the meta-data.
     *
     * @param targetClass Type to which the return value must be casted.
     * @param <T> generic type
     * @return the meta-data or a data-structure which represents the meta-data.
     */
    public <T> T getValue(Class<T> targetClass)
    {
        return (T)getValue();
    }

    /**
     * Sets the object which represents the meta-data hold by this instance.
     *
     * @param value the object which represents the meta-data hold by this instance.
     */
    public void setValue(Object value)
    {
        this.logger.finest("setting meta-data value: " + value);

        this.value = value;
    }

    /**
     * Sets the map which contains further properties which are linked to the meta-data.
     *
     * @param properties properties which are linked to the meta-data.
     */
    public void setProperties(Map<String, Object> properties)
    {
        this.properties = properties;
    }

    /**
     * Returns the property-value for the given property-key.
     *
     * @param key key value used to identify the property value.
     * @return property value for the specified key.
     */
    public Object getProperty(String key)
    {
        return this.properties.get(key);
    }

    /**
     * Returns the property-value for the given property-key.
     *
     * @param key key which identifies a property
     * @param targetClass Type to which the return value must be casted.
     * @param <T> Generic type
     * @return property value for the specified key.
     */
    public <T> T getProperty(String key, Class<T> targetClass)
    {
        return (T)getProperty(key);
    }

    /**
     * Sets the property-value for the given property-key.
     * {@link org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys}
     * contains the keys used by ExtVal.
     *
     * @param key key value used to identify the property value.
     * @param value property value to set.
     */
    public void setProperty(String key, Object value)
    {
        this.logger.finest("new property added key: " + key + " value: " + value + " for metadata-key: " + this.key);

        this.properties.put(key, value);
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (!(o instanceof MetaDataEntry))
        {
            return false;
        }

        MetaDataEntry that = (MetaDataEntry) o;

        if (key != null ? !key.equals(that.key) : that.key != null)
        {
            return false;
        }
        if (properties != null ? !properties.equals(that.properties) : that.properties != null)
        {
            return false;
        }
        if (value != null ? !value.equals(that.value) : that.value != null)
        {
            return false;
        }

        return true;
    }

    @Override
    public int hashCode()
    {
        int result = key != null ? key.hashCode() : 0;
        result = 31 * result + (value != null ? createNullAwareHashCode(value) : 0);
        result = 31 * result + (properties != null ? createNullAwareHashCode(properties) : 0);
        return result;
    }

    private int createNullAwareHashCode(Object o)
    {
        try
        {
            return o.hashCode();
        }
        catch (NullPointerException e)
        {
            return 0;
        }
    }
}
