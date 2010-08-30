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
 * Data holder which stores the meta-data and some information where the meta-data was around.
 *
 * @author Gerhard Petracek
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
     * Returns the key of the meta-data.
     *
     * @return key of the meta-data.
     */
    public String getKey()
    {
        return key;
    }

    /**
     * Sets the key of the meta-data.  The standard modules of ExtVal uses the name of the annotation as key.
     *
     * @param key value to set as key of the meta-data.
     */
    public void setKey(String key)
    {
        this.logger.finest("setting meta-data key: " + key);

        this.key = key;
    }

    /**
     * Returns the object that lead to the creation of this instance.
     *
     * @return main object for the meta-data instance.
     */
    public Object getValue()
    {
        return value;
    }

    /**
     * Returns the object that lead to the creation of this instance casted to the specified type.  Is useful for
     * meta-data created for Bean Validation annotations, since the main object there is a ConstraintDescriptor.
     *
     * @param targetClass Type to which the return value must be casted.
     * @param <T> generic type
     * @return main object for the meta-data instance.
     */
    public <T> T getValue(Class<T> targetClass)
    {
        return (T)getValue();
    }

    /**
     * Sets the object that lead to the creation of this meta-data instance. The standard modules stores the annotation
     * or the ConstraintDescriptor that lead to the creation of this instance.
     *
     * @param value main object for the meta-data instance.
     */
    public void setValue(Object value)
    {
        this.logger.finest("setting meta-data value: " + value);

        this.value = value;
    }

    /**
     * Sets the map instance where the additional properties are stored of the meta-data.
     *
     * @param properties Map with properties of the meta-data.
     */
    public void setProperties(Map<String, Object> properties)
    {
        this.properties = properties;
    }

    /**
     * Returns the value defined in the additional properties identified by the key value.  When there is no value
     * defined for the key, returns null.
     *
     * @param key key value used to identify the property value.
     * @return property value for the specified key.
     */
    public Object getProperty(String key)
    {
        return this.properties.get(key);
    }

    /**
     * Returns the value defined in the additional properties identified by the key value casted to the specified type.
     *
     * @param key key value used to identify the property value.
     * @param targetClass Type to which the return value must be casted.
     * @param <T> Generic type
     * @return property value for the specified key.
     */
    public <T> T getProperty(String key, Class<T> targetClass)
    {
        return (T)getProperty(key);
    }

    /**
     * Adds the property value with the key to the list of properties.  The constants defined in PropertyInformationKeys
     *  can be used as key for the storage.
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
