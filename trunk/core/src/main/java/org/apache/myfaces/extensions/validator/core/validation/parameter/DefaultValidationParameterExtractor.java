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
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
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
        return extractById(annotation, null);
    }

    public List<Object> extract(Annotation annotation, Object key)
    {
        return extractById(annotation, key, null);
    }

    public <T> List<T> extract(Annotation annotation, Object key, Class<T> valueType)
    {
        return extractById(annotation, key, valueType, null);
    }

    public <T> T extract(Annotation annotation, Object key, Class<T> valueType, Class valueId)
    {
        List<T> results = extractById(annotation, key, valueType, valueId);

        if(results.iterator().hasNext())
        {
            return results.iterator().next();
        }

        return null;
    }

    @SuppressWarnings({"unchecked"})
    public <T> List<T> extractById(Annotation annotation, Object key, Class<T> valueType, Class valueId)
    {
        List<Object> result = new ArrayList<Object>();

        for(Object entry : extractById(annotation, key, valueId))
        {
            if(valueType.isAssignableFrom(entry.getClass()))
            {
                result.add(entry);
            }
        }

        return (List<T>)result;
    }

    public List<Object> extractById(Annotation annotation, Object key, Class valueId)
    {
        Map<Object, List<Object>> fullResult = extractById(annotation, valueId);

        if(fullResult.containsKey(key))
        {
            return fullResult.get(key);
        }

        return new ArrayList<Object>();
    }

    @ToDo(value = Priority.MEDIUM, description = "add web.xml parameter for performance tuning to deactivate the scan")
    public Map<Object, List<Object>> extractById(Annotation annotation, Class valueId)
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
                        processParameterValue(annotation, currentParameterValue, result, valueId);
                    }
                }
                else if(parameterValue instanceof Class)
                {
                    //keep check so that following is true:
                    //if at least one parameter is found which tells that it isn't a blocking error, let it pass
                    processParameterValue(annotation, (Class)parameterValue, result, valueId);
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

    private void processParameterValue(
            Annotation annotation, Class paramClass, Map<Object, List<Object>> result, Class valueId) throws Exception
    {
        Object key = null;
        List<Object> parameterValues = new ArrayList<Object>();

        if(ValidationParameter.class.isAssignableFrom(paramClass))
        {
            //support pure interface approach e.g. ViolationSeverity.Warn.class
            for(Field currentField : paramClass.getDeclaredFields())
            {
                key = processFoundField(annotation, currentField, parameterValues, key, valueId);
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
                    key = processFoundMethod(paramClass, currentMethod, parameterValues, key, valueId);
                }

                for(Field currentField : currentInterface.getDeclaredFields())
                {
                    key = processFoundField(annotation, currentField, parameterValues, key, valueId);
                }
            }

            //inspect the other methods of the implementing class
            for(Method currentMethod : paramClass.getDeclaredMethods())
            {
                processFoundMethod(paramClass, currentMethod, parameterValues, key, valueId);
            }
        }

        key = createDefaultKey(key, paramClass);

        if(parameterValues.isEmpty())
        {
            //@ParameterValue is optional as well
            parameterValues.add(key);
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

    private Object createDefaultKey(Object key, Class currentClass)
    {
        if(key == null)
        {
            //check for super-interface (exclude ValidationParameter itself)
            for(Class interfaceClass : currentClass.getInterfaces())
            {
                if(ValidationParameter.class.isAssignableFrom(interfaceClass) &&
                        (!interfaceClass.getName().equals(ValidationParameter.class.getName())))
                {
                    key = interfaceClass;
                    break;
                }
            }
        }

        if(key == null)
        {
            key = currentClass;
        }

        return key;
    }

    private Object processFoundField(
            Object instance, Field currentField, List<Object> paramValues, Object key, Class valueId)
    {
        Object newKey = null;
        if(key == null && currentField.isAnnotationPresent(ParameterKey.class))
        {
            try
            {
                newKey = currentField.get(instance);
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
            if(valueId == null || valueId.equals(currentField.getAnnotation(ParameterValue.class).id()))
            {
                currentField.setAccessible(true);
                try
                {
                    paramValues.add(currentField.get(instance));
                }
                catch (Throwable e)
                {
                    if(this.logger.isWarnEnabled())
                    {
                        this.logger.warn(e);
                    }
                }
            }
        }

        return newKey != null ? newKey : key;
    }

    private Object processFoundMethod(
            Class paramClass, Method currentMethod, List<Object> parameterValues, Object key, Class valueId)
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
            if(valueId == null || valueId.equals(currentMethod.getAnnotation(ParameterValue.class).id()))
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
                            //for attributes like: Class<? extends InheritedFromValidationParameter> value();
                            ValidationParameter.class.isAssignableFrom((Class)upperBounds))
                    {
                        return true;
                    }
                }
            }
        }

        return false;
    }
}