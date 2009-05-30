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
package org.apache.myfaces.extensions.validator.core.validation.parameter;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.util.ArrayList;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.WildcardType;

/**
 * @author Gerhard Petracek
 * @since 1.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultValidationParameterExtractor implements ValidationParameterExtractor
{
    protected final Log logger = LogFactory.getLog(getClass());

    public Map<Object, List<Object>> extract(Annotation annotation)
    {
        Map<Object, List<Object>> result = new HashMap<Object, List<Object>>();

        for(Method currentAnnotationAttribute : annotation.annotationType().getDeclaredMethods())
        {
            try
            {
                if(!isValidationParameter(currentAnnotationAttribute.getGenericReturnType()))
                {
                    continue;
                }

                Object parameterValue = currentAnnotationAttribute.invoke(annotation);

                if(parameterValue instanceof Class[])
                {
                    for(Class currentParameterValue : (Class[])parameterValue)
                    {
                        //keep check so that following is true:
                        //if at least one parameter is found which tells that it isn't a blocking error, let it pass
                        processParameterValue(annotation, currentParameterValue, result);
                    }
                }
                else if(parameterValue instanceof Class)
                {
                    //keep check so that following is true:
                    //if at least one parameter is found which tells that it isn't a blocking error, let it pass
                    processParameterValue(annotation, (Class)parameterValue, result);
                }
            }
            catch (Throwable e)
            {
                if(this.logger.isWarnEnabled())
                {
                    this.logger.warn(e);
                }
            }
        }

        return result;
    }

    public List<Object> extract(Annotation annotation, Object key)
    {
        Map<Object, List<Object>> fullResult = extract(annotation);

        if(fullResult.containsKey(key))
        {
            return fullResult.get(key);
        }

        return new ArrayList<Object>();
    }

    @SuppressWarnings({"unchecked"})
    public <T> List<T> extract(Annotation annotation, Object key, Class<T> valueType)
    {
        List<Object> result = new ArrayList<Object>();

        for(Object entry : extract(annotation, key))
        {
            if(valueType.isAssignableFrom(entry.getClass()))
            {
                result.add(entry);
            }
        }

        return (List<T>)result;
    }

    private void processParameterValue(Annotation annotation, Class paramClass, Map<Object, List<Object>> result)
            throws Exception
    {
        Object key = null;
        List<Object> parameterValues = new ArrayList<Object>();

        if(ValidationParameter.class.isAssignableFrom(paramClass))
        {
            //support pure interface approach e.g. ViolationSeverity.Warn.class
            for(Field currentField : paramClass.getDeclaredFields())
            {
                key = processFoundField(annotation, currentField, parameterValues, key);
            }

            for(Class currentInterface : paramClass.getInterfaces())
            {
                if(!ValidationParameter.class.isAssignableFrom(currentInterface))
                {
                    continue;
                }

                //support interface + impl. approach e.g. MyParamImpl.class
                //(MyParamImpl implements MyParam
                //MyParam extends ValidationParameter
                //methods in the interface have to be marked with @ParameterValue and @ParameterKey
                for(Method currentMethod : currentInterface.getDeclaredMethods())
                {
                    key = processFoundMethod(paramClass, currentMethod, parameterValues, key);
                }

                for(Field currentField : currentInterface.getDeclaredFields())
                {
                    key = processFoundField(annotation, currentField, parameterValues, key);
                }
            }
        }

        if(key == null)
        {
            key = paramClass;
        }

        if(result.containsKey(key))
        {
            result.get(key).addAll(parameterValues);
        }
        else
        {
            result.put(key, parameterValues);
        }
    }

    private Object processFoundField(Annotation annotation, Field currentField, List<Object> paramValues, Object key)
    {
        Object newKey = null;
        if(key == null && currentField.isAnnotationPresent(ParameterKey.class))
        {
            try
            {
                newKey = currentField.get(annotation);
            }
            catch (Throwable e)
            {
                if(this.logger.isWarnEnabled())
                {
                    this.logger.warn(e);
                }
            }
        }
        //no "else if" to allow both at one field
        if(currentField.isAnnotationPresent(ParameterValue.class))
        {
            currentField.setAccessible(true);
            try
            {
                paramValues.add(currentField.get(annotation));
            }
            catch (Throwable e)
            {
                if(this.logger.isWarnEnabled())
                {
                    this.logger.warn(e);
                }
            }
        }

        return newKey != null ? newKey : key;
    }

    private Object processFoundMethod(Class paramClass, Method currentMethod, List<Object> parameterValues, Object key)
    {
        Object newKey = null;
        if(key == null && currentMethod.isAnnotationPresent(ParameterKey.class))
        {
            try
            {
                newKey = currentMethod.invoke(paramClass.newInstance());
            }
            catch (Throwable e)
            {
                if(this.logger.isWarnEnabled())
                {
                    this.logger.warn(e);
                }
            }
        }
        //no "else if" to allow both at one field
        if(currentMethod.isAnnotationPresent(ParameterValue.class))
        {
            currentMethod.setAccessible(true);
            try
            {
                parameterValues.add(currentMethod.invoke(paramClass.newInstance()));
            }
            catch (Throwable e)
            {
                if(this.logger.isWarnEnabled())
                {
                    this.logger.warn(e);
                }
            }
        }

        return newKey != null ? newKey : key;
    }

    private boolean isValidationParameter(Type genericReturnType)
    {
        if(genericReturnType instanceof GenericArrayType)
        {
            if(((GenericArrayType)genericReturnType).getGenericComponentType() instanceof ParameterizedType)
            {
                return analyzeParameterizedType(
                        (ParameterizedType)((GenericArrayType)genericReturnType).getGenericComponentType());
            }
        }
        else if(genericReturnType instanceof ParameterizedType)
        {
            return analyzeParameterizedType(
                    (ParameterizedType)genericReturnType);
        }

        return false;
    }

    private boolean analyzeParameterizedType(ParameterizedType parameterizedType)
    {
        for(Type type : parameterizedType.getActualTypeArguments())
        {
            if(type instanceof WildcardType)
            {
                for(Type upperBounds : ((WildcardType)type).getUpperBounds())
                {
                    if(upperBounds instanceof Class &&
                            ((Class)upperBounds).isAssignableFrom(ValidationParameter.class))
                    {
                        return true;
                    }
                }
            }
        }

        return false;
    }
}