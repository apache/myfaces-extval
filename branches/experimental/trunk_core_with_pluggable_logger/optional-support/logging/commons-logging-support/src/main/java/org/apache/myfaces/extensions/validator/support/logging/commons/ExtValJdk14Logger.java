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
package org.apache.myfaces.extensions.validator.support.logging.commons;

import java.io.Serializable;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.logging.Log;
import org.apache.myfaces.extensions.validator.util.ClassUtils;


/**
 * @author Gerhard Petracek
 * @since 1.x.1
 * 
 * <p/>
 * original impl. with dirty hack was implemented by:
 * @author <a href="mailto:sanders@apache.org">Scott Sanders</a>
 * @author <a href="mailto:bloritsch@apache.org">Berin Loritsch</a>
 * @author <a href="mailto:donaldp@apache.org">Peter Donald</a>
 * @version $Revision: 424107 $ $Date: 2006-07-21 01:15:42 +0200 (Fr, 21 Jul 2006) $
 */

public class ExtValJdk14Logger implements Log, Serializable
{

    /**
     * This member variable simply ensures that any attempt to initialise
     * this class in a pre-1.4 JVM will result in an ExceptionInInitializerError.
     * It must not be private, as an optimising compiler could detect that it
     * is not used and optimise it away.
     */
    protected static final Level DUMMY_LEVEL = Level.FINE;

    // ----------------------------------------------------------- Constructors


    /**
     * Construct a named instance of this Logger.
     *
     * @param name Name of the logger to be constructed
     */
    public ExtValJdk14Logger(String name)
    {

        this.name = name;
        logger = getLogger();

    }

    // ----------------------------------------------------- Instance Variables


    /**
     * The underlying Logger implementation we are using.
     */
    protected transient Logger logger = null;


    /**
     * The name of the logger we are wrapping.
     */
    protected String name = null;


    /**
     * customized method
     */
    private void log(Level level, String msg, Throwable ex)
    {

        Logger logger = getLogger();
        if (logger.isLoggable(level))
        {
            //customized:
            Throwable dummyException = new Throwable();
            StackTraceElement locations[] = dummyException.getStackTrace();
            // Caller will be the third element
            String cname = "unknown";
            String method = "unknown";
            if (locations != null && locations.length > 2)
            {
                String[] caller = extractCaller(locations);
                cname = caller[0];
                method = caller[1];
            }
            if (ex == null)
            {
                logger.logp(level, cname, method, msg);
            }
            else
            {
                logger.logp(level, cname, method, msg, ex);
            }
        }

    }

    /**
     * new method for extval
     */
    private String[] extractCaller(StackTraceElement[] locations)
    {
        Class classToTest;
        Class targetClass = ClassUtils.tryToLoadClassForName(this.logger.getName());
        String result[] = new String[2];

        //fallback (= original hack of commons logging)
        result[0] = locations[2].getClassName();
        result[1] = locations[2].getMethodName();

        //name is no class name -> use fallback of the original implementation
        if(targetClass == null)
        {
            return result;
        }

        /*
         * the rest is to find the called method
         */

        /*
         * shortcut if LogUtils are used
         */
        classToTest = ClassUtils.tryToLoadClassForName(locations[6].getClassName());

        //most common case: LogUtils and there is no exception to log
        if(classToTest != null && classToTest.isAssignableFrom(targetClass))
        {
            result[0] = locations[6].getClassName();
            result[1] = locations[6].getMethodName();
            return result;
        }

        classToTest = ClassUtils.tryToLoadClassForName(locations[5].getClassName());

        //common case: LogUtils and there is an exception to log
        if(classToTest != null && classToTest.isAssignableFrom(targetClass))
        {
            result[0] = locations[5].getClassName();
            result[1] = locations[5].getMethodName();
            return result;
        }

        /*
         * uncommon case: LogUtils aren't used -> use a generic approach to find the method name
         */

        int i = 0;
        for (StackTraceElement stackTraceElement : locations)
        {
            classToTest = ClassUtils.tryToLoadClassForName(stackTraceElement.getClassName());

            if(classToTest == null)
            {
                continue;
            }

            if(classToTest.isAssignableFrom(targetClass))
            {
                result[0] = locations[i].getClassName();
                result[1] = locations[i].getMethodName();
                return result;
            }
            i++;
        }

        result[1] = "unknown";
        return result;
    }

    /**
     * Logs a message with <code>java.util.logging.Level.FINE</code>.
     *
     * @param message to log
     * @see org.apache.commons.logging.Log#debug(Object)
     */
    public void debug(Object message)
    {
        log(Level.FINE, String.valueOf(message), null);
    }


    /**
     * Logs a message with <code>java.util.logging.Level.FINE</code>.
     *
     * @param message   to log
     * @param exception log this cause
     * @see org.apache.commons.logging.Log#debug(Object, Throwable)
     */
    public void debug(Object message, Throwable exception)
    {
        log(Level.FINE, String.valueOf(message), exception);
    }


    /**
     * Logs a message with <code>java.util.logging.Level.SEVERE</code>.
     *
     * @param message to log
     * @see org.apache.commons.logging.Log#error(Object)
     */
    public void error(Object message)
    {
        log(Level.SEVERE, String.valueOf(message), null);
    }


    /**
     * Logs a message with <code>java.util.logging.Level.SEVERE</code>.
     *
     * @param message   to log
     * @param exception log this cause
     * @see org.apache.commons.logging.Log#error(Object, Throwable)
     */
    public void error(Object message, Throwable exception)
    {
        log(Level.SEVERE, String.valueOf(message), exception);
    }


    /**
     * Logs a message with <code>java.util.logging.Level.SEVERE</code>.
     *
     * @param message to log
     * @see org.apache.commons.logging.Log#fatal(Object)
     */
    public void fatal(Object message)
    {
        log(Level.SEVERE, String.valueOf(message), null);
    }


    /**
     * Logs a message with <code>java.util.logging.Level.SEVERE</code>.
     *
     * @param message   to log
     * @param exception log this cause
     * @see org.apache.commons.logging.Log#fatal(Object, Throwable)
     */
    public void fatal(Object message, Throwable exception)
    {
        log(Level.SEVERE, String.valueOf(message), exception);
    }


    /**
     * Return the native Logger instance we are using.
     */
    public Logger getLogger()
    {
        if (logger == null)
        {
            logger = Logger.getLogger(name);
        }
        return (logger);
    }


    /**
     * Logs a message with <code>java.util.logging.Level.INFO</code>.
     *
     * @param message to log
     * @see org.apache.commons.logging.Log#info(Object)
     */
    public void info(Object message)
    {
        log(Level.INFO, String.valueOf(message), null);
    }


    /**
     * Logs a message with <code>java.util.logging.Level.INFO</code>.
     *
     * @param message   to log
     * @param exception log this cause
     * @see org.apache.commons.logging.Log#info(Object, Throwable)
     */
    public void info(Object message, Throwable exception)
    {
        log(Level.INFO, String.valueOf(message), exception);
    }


    /**
     * Is debug logging currently enabled?
     */
    public boolean isDebugEnabled()
    {
        return (getLogger().isLoggable(Level.FINE));
    }


    /**
     * Is error logging currently enabled?
     */
    public boolean isErrorEnabled()
    {
        return (getLogger().isLoggable(Level.SEVERE));
    }


    /**
     * Is fatal logging currently enabled?
     */
    public boolean isFatalEnabled()
    {
        return (getLogger().isLoggable(Level.SEVERE));
    }


    /**
     * Is info logging currently enabled?
     */
    public boolean isInfoEnabled()
    {
        return (getLogger().isLoggable(Level.INFO));
    }


    /**
     * Is trace logging currently enabled?
     */
    public boolean isTraceEnabled()
    {
        return (getLogger().isLoggable(Level.FINEST));
    }


    /**
     * Is warn logging currently enabled?
     */
    public boolean isWarnEnabled()
    {
        return (getLogger().isLoggable(Level.WARNING));
    }


    /**
     * Logs a message with <code>java.util.logging.Level.FINEST</code>.
     *
     * @param message to log
     * @see org.apache.commons.logging.Log#trace(Object)
     */
    public void trace(Object message)
    {
        log(Level.FINEST, String.valueOf(message), null);
    }


    /**
     * Logs a message with <code>java.util.logging.Level.FINEST</code>.
     *
     * @param message   to log
     * @param exception log this cause
     * @see org.apache.commons.logging.Log#trace(Object, Throwable)
     */
    public void trace(Object message, Throwable exception)
    {
        log(Level.FINEST, String.valueOf(message), exception);
    }


    /**
     * Logs a message with <code>java.util.logging.Level.WARNING</code>.
     *
     * @param message to log
     * @see org.apache.commons.logging.Log#warn(Object)
     */
    public void warn(Object message)
    {
        log(Level.WARNING, String.valueOf(message), null);
    }


    /**
     * Logs a message with <code>java.util.logging.Level.WARNING</code>.
     *
     * @param message   to log
     * @param exception log this cause
     * @see org.apache.commons.logging.Log#warn(Object, Throwable)
     */
    public void warn(Object message, Throwable exception)
    {
        log(Level.WARNING, String.valueOf(message), exception);
    }


}