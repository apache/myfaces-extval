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
package org.apache.myfaces.extensions.validator.test.core.config;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.storage.PropertyStorage;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAware2MetaDataBean;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAwareBean;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAwareMetaDataBean;
import org.apache.myfaces.extensions.validator.test.core.config.support.CustomConstraintSource;
import org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationConstraintSourceAnnotationTestCase extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationConstraintSourceAnnotationTestCase(String name)
    {
        super(name);
    }

    @Override
    protected ExtValCoreConfiguration getCustomExtValCoreConfiguration()
    {
        if (needCustomConfig())
        {
            return new DefaultExtValCoreConfiguration()
            {
                @Override
                public Class<? extends Annotation> constraintSourceAnnotation()
                {
                    return CustomConstraintSource.class;
                }

            };
        }
        else
        {
            return null;
        }
    }

    public void testConstraintSourceAnnotationDefault()
    {
        // Using the ConstraintSourceUtils.resolveMappedConstraintSourceFor
        // needs to much setup.

        Method method = ReflectionUtils.tryToGetMethod(ConstraintSourceUtils.class, "findMappedClass",
                PropertyStorage.class, Class.class, String.class);
        assertNotNull(method);
        method.setAccessible(true);
        Object[] args = new Object[3];
        args[0] = ReflectionUtils.getPropertyStorage();
        args[1] = ConstraintSourceAwareBean.class;
        args[2] = "property2";
        Object result = ReflectionUtils.tryToInvokeMethodOfClass(ConstraintSourceUtils.class, method, args);
        assertNotNull(result);
        assertEquals(ConstraintSourceAwareMetaDataBean.class, result);

    }

    public void testConstraintSourceAnnotationCustomConfig()
    {
        Method method = ReflectionUtils.tryToGetMethod(ConstraintSourceUtils.class, "findMappedClass",
                PropertyStorage.class, Class.class, String.class);
        assertNotNull(method);
        method.setAccessible(true);
        Object[] args = new Object[3];
        args[0] = ReflectionUtils.getPropertyStorage();
        args[1] = ConstraintSourceAwareBean.class;
        args[2] = "property2";
        Object result = ReflectionUtils.tryToInvokeMethodOfClass(ConstraintSourceUtils.class, method, args);
        assertNotNull(result);
        assertEquals(ConstraintSourceAware2MetaDataBean.class, result);
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationConstraintSourceAnnotationTestCase.class);
    }

}
